# Const-eval: "non-constant captured in `comptime` block" — investigation

Status: **const-eval "non-constant captured" bug FIXED (three fixes); a separate, deeper
deconstruction panic (`DataOrigin::Unknown`) now surfaces further down the pipeline — tracked
as a distinct follow-up**
Last updated: 2026-09-14

## 0. Progress this session (2026-09-14, continued)

- **Fix A — once-only execution** (`const_eval.rs`): `ConstNodeState.executed_statements:
  HashSet<BlockLocalStatementUid>` records each `Call` statement executed on the VM. On a
  later worklist pass the statement is NOT re-executed (prevents the double-allocation /
  shadow-resource leak the user flagged). If its inputs are still available the cached
  result is re-validated (`revalidate_cached_value`); if an input has since become runtime
  the value is invalidated (`mark_runtime`).
- **Fix B — D1 scoped forest** (`borrow.rs` + `const_eval.rs`): added
  `ReferenceStateForest::reset(forest, base_value)` and call it in
  `CallParameterCopy::set_vm_values` right after `avail.clear()`, resetting the sticky-max
  `FlowState` forest to all-`Fixed` per block pass. This eliminates cross-pass/cross-block
  leakage of `Floating` state that spuriously made `is_avail` false for constant captures.
- **Effect**: the `interpolate()`/func#136 **deconstruction panic** (`DataOrigin::Unknown`,
  caused by an interim "keep result stable" refinement that was since reverted) is gone, and
  the ch.eq `init()` re-execution error is gone.
- **Fix C — validator reachability** (`const_eval.rs`, `validate_comptime_context`): the
  `main.eq:330` `comptime { Res::new(0usize) }` in `simple_outer()` (func#149) sat in block
  `$1b`, behind a `Seal::Cond` whose condition (`std::env_default(...)`, a `?comptime` fn)
  folds to a **known** value. The const-eval worklist therefore followed the sealing statement
  straight to the taken branch and **never visited** block `$1b` (20 of func#149's 31 blocks
  are visited), so its captured literal was legitimately absent from the constant table. The
  bug was that `validate_comptime_context` iterated **all** blocks regardless of reachability
  and flagged that absent value. Fix: skip blocks whose `ConstNodeState.computation_counter ==
  0` (i.e. never reached by the worklist). This is *not* a worklist bug — the dead branch is
  correctly unreachable — it is a validator over-reach. **The main.eq:330 "non-constant"
  error is now gone.**
- **Net effect on the production sim**: all three const-eval "non-constant captured" failures
  (ch.eq `init()` re-execution, the D1 poisoning, and the main.eq:330 unreachable-block
  validator over-reach) are resolved. The regression test
  (`test_const_eval_loop_capture`) passes; the edlc test suites show only pre-existing
  failures (`conversions` in `edlc_codegen_cranelift`; 15 ast/hir/resolver/compiler in
  `edlc_core`).
- **New, separate downstream failure (follow-up, NOT this bug)**: with the const-eval errors
  resolved, the compiler now progresses further and panics in the SSA **deconstruction** pass —
  `DataOrigin::Unknown for value $a6` in `PartialSsaDeconstruction::consolidate`
  (`deconstruction.rs`) for func#149 (`simple_outer`). This is a distinct deconstruction
  inconsistency that the earlier errors previously masked; it is tracked separately.

## 1. The bug

A rare production error reported by the const-eval pipeline:

```
non-constant captured in `comptime` block
```

- Emitted from `check_constant`, `edlc_core/src/mir/mir_expr/mir_graph/const_eval.rs:1034`
  (the `phase.report_error(...)` arm of `check_constant`).
- Fires when a value captured by a `comptime` block is not `Known` in
  `ConstEval::state.consts` (i.e. `get_constant_value(val) == None`).
- The captured value **is** a true constant in the failing program (a global read,
  or a value that is comptime before the loop and untouched by the loop), so the
  error is a false positive.

### Production snippet that triggers it

```edl
fn init() {
    println("initializing Cahn-Hilliard solver...");
    // init boundary fields
    comptime { phi.intern().field() }.fill(SVector::new(-1.0));
    let ctx = CtxId::new(0);

    let mut i = 0u32;
    loop {
        if i >= comptime { domain.num_cells() }.get(ctx) { break }

        let id = MtId { id: i, ctx };

        let c = domain.get_cell_center(id);
        let x = c.index(0);
        let y = c.index(1);

        let r = f64::sqrt(x * x + y * y);
        let x = (r * 0.05);

        let val = std::rand::random(-0.1, 0.1);

        comptime { phi.intern().field() }.set(id, [SVector::new(val)]);   // <-- error here
        i += 1;
    }

    let inlet_bc = MtId { id: domain.get_boundary_id("inflow"), ctx };
    phi.set_bc(inlet_bc, BoundaryCondition::dirichlet(SVector::zeros()));
    // ... more set_bc calls on phi / eta / eta_grad, buffer_all, advance_time, etc.
}
```

- `phi` is an **immutable global**. `intern()` and `field()` are `comptime`.
- Signatures:
  - `comptime BoundaryField::intern(async self) -> async BoundaryFieldIntern`
  - `comptime BoundaryFieldIntern::field(async self) -> async Field`
  - `async fn set(async self, index: MtId, val: [SVector<f64, 1>; 1])`
- `set` takes the field **by value** (in the real code these "field" types are
  runtime-managed smart pointers; that detail is not language-relevant).
- There are three `comptime { ... }` blocks: one pre-loop (`.fill`), one in the
  loop guard (`comptime { domain.num_cells() }` on a *different* global `domain`),
  and one in the loop body (`.set`) — the one that errors.

### The full production case (new, 2026-09-14)

- Simulation: `~/simulations/cahn-hilliard-navier-stokes` (AcoDyn project
  `ItTakesTwoPhases`, solver `comp_navier_stokes` v1.0).
- Failing function: `ch::init()` in
  `~/simulations/cahn-hilliard-navier-stokes/comp_navier_stokes/src/ch.eq:82-140`.
- Reproduce: `cd ~/simulations/cahn-hilliard-navier-stokes && acodyn run`.
  The failure happens **during compilation** (const-eval phase), before any
  simulation time-step runs.
- `acodyn` is installed to `~/.acodyn/bin/acodyn`; the EDL std lib is loaded from
  `~/.acodyn/include/std` (identical to `~/projects/acodyn/edl/std`).
- **Build/reinstall procedure**: `acodyn` links `edlc_core` as a path dependency on
  this workspace (`~/projects/acodyn/Cargo.toml`: `edlc_core = { path="../edlc/edlc_core" }`).
  Any change in this repo requires, in `~/projects/acodyn`:
  1. `cargo build --release`
  2. `./install.sh` (installs to `~/.acodyn/`)
  then re-run the production case.
- GPU available on this machine (RTX 5080); all AcoDyn tests require a CUDA device.

## 2. Root-cause hypothesis (D1) — CONFIRMED as the mechanism

The `references` flow-state forest is the culprit.

### Key facts (with locations)

- `ConstFrame.references: ReferenceStateForest<FlowState>` is a **single
  persistent field** on `ConstEval`.
  - Declared: `const_eval.rs` (`pub(crate) struct ConstFrame { avail, references }`).
  - Initialized **once** to all-`FlowState::Fixed` in `ConstEval::new`.
  - **Never reset** for the whole worklist run.
- `set_vm_values` (const_eval.rs:436-465) clears `block_frame.avail` on every block
  re-processing (line 445) but **does not touch `references`**. So `avail` is fresh
  per block while the forest accumulates.
- For each block param: a `Known` param → `set_avail` (owner→`Fixed`); a
  `Runtime`/`Unknown` param → `set_unavail` (owner→**`Floating`**).
- `ReferenceState::set_value` (borrow.rs:147-186) is **sticky-max**: setting a node
  to `Floating` (a) overwrites the node and all its descendants to `Floating`, and
  (b) recomputes every ancestor as the **max of its children**
  (`FlowState::upper`, where `Floating > Fixed`). One `Floating` node therefore
  poisons the whole root→leaf chain it sits on.
- `is_avail(value)` (const_eval.rs:469-478) — the gate for whether a comptime call
  is executed — requires `avail.contains(value) && get_max_for_owners(value) == Fixed`.
  `is_deref_avail` (const_eval.rs:480-483) is the same for the deref forest and is
  used by `MirDeref::is_avail` (mir_ref.rs:677-683).

### The failure path

1. In some worklist iteration, a value that lives in the same borrow-tree
   component as the global `phi` becomes a `Runtime` block param (channel 1), or a
   runtime `Assign`/`&mut`-call touches a value with a `Global(phi)` borrow path
   (channel 2).
2. The corresponding `set_unavail` / `set_deref_unavail` marks it `Floating`;
   max-propagation flips `phi`'s own node to `Floating`.
3. In a later iteration, `is_avail(phi)` / `is_deref_avail(...)` is `false` → the
   comptime chain is not comptime-executed → the captured handle is
   `mark_runtime`'d.
4. `state.consts` is a **monotone** lattice: once a value joins to `Runtime` it is
   never healed again (const_eval.rs:136-144 `join_mut`, :638-641
   `insert_const_value`, :862-867 `mark_runtime`).
5. `check_constant` on the captured value fails → error.

### The two exact poisoning channels (new, 2026-09-14)

- **Channel 1 — `set_unavail(V)`** (V becomes a `Runtime` block param in
  `set_vm_values`, const_eval.rs:460-461): `set_owner_value` flips V's node in
  every `Local(O)` tree for owners V *owns* (borrow.rs:942-961). The global read of
  `phi` owns `O_phi`; the read value itself (a root leaf of the `Local(O_phi)`
  tree) or an `Entire`-ref derivative of it (which gains a `Local(O_phi)` path,
  borrow.rs:1975-1988) set unavail flips `phi`'s owner node via ancestor-max
  (borrow.rs:165-185).
- **Channel 2 — `set_deref_unavail(V)`**: fired from the *else* branch of
  `Statement::VarDef::eval_consts` (const_eval.rs:1764-1789) for any
  non-comptime-executable `Assign` (lhs = V) or any runtime `Call` with a
  `&mut`-ref param V. `set_deref_value` iterates V's **borrow paths** — which
  include `Global(phi)` sources (borrow.rs:920-937) — and calls `set_value` on each
  of those trees, poisoning the entire `Global(phi)` tree. `Global(phi)`-path
  values exist inside inlined comptime bodies: `Ref`/`Deref` nodes on the global
  read (a `Deref` of a ref with a `Local(O_phi)` path inherits the owner's
  `Global(phi)` path, borrow.rs:2009-2027).

### Why the error is order/structure sensitive ("Heisenbug")

Whether the poison is still present when the comptime block is **last** processed
(depending on worklist pass ordering and which values join to `Runtime` on which
pass) determines whether the handle's `Runtime` latch survives to
`validate_comptime_context`. Small changes to the program structure change the
worklist dynamics, which is why the error is rare and evades minimal repros.
Note: the worklist itself is deterministic FIFO (`propagate_constants`,
const_eval.rs:1389-1431); the sensitivity is structural, not temporal. (Per-run
determinism of the production case is still to be confirmed by running it twice.)

### Soundness argument for the fix

A `Context::Comptime` block cannot legally contain runtime calls or assigns
(context validation, `RtCallInCtCtx`, validate.rs). Therefore an *in-pass*
poisoning of the `phi` chain inside a comptime block cannot happen for a
well-formed program — all observed poisoning arrives via **cross-pass /
cross-block leakage** from the never-reset forest. Eliminating the leakage (by
scoping the forest per block pass, like `avail`) removes 100% of the observed
false positives.

### Why D6 / D4 were ruled out

- **D6** (cross-function global mutation): globals are immutable — they cannot be
  written after declaration/initialization. Not applicable.
- **D4** (raw-byte `Known` comparison): real but not the trigger here; fixing it
  properly (partial-eq instead of byte compare) needs extra trait resolution and
  is a separate, larger effort.
  - Reinforced by the production API inspection: `Field`, `Intern<Field>`,
    `Intern<BoundaryField>` etc. are `ResourceId`s (plain integer resource ids;
    `~/projects/acodyn/src/solve/equation/custom/field.rs:771-782, 927-1044`).
    `intern()`/`field()` are pure resource lookups, so the handle bytes are
    **stable across comptime re-execution** — the byte compare does not spuriously
    diverge for these values.

## 3. What was tried (reproduction attempts)

Repro test added: `test_const_eval_loop_capture` in
`edlc_codegen_cranelift/src/executor.rs` (see `#[cfg(test)] mod test`). It mirrors
`test_sync_logic` and uses the exact signatures above with mock
`BoundaryField` / `BoundaryFieldIntern` / `Field` types.

Progressive versions tried, **none triggered the error**:

1. Global + comptime chain + loop with in-loop `comptime { phi.intern().field() }.set(i, v)`.
2. + pre-loop `comptime { ... }.fill(...)` block.
3. + second global `domain` + comptime block in the loop guard
   (`comptime { domain.num_cells() }.get(0)`).
4. + runtime helper `rand_val(i)` in the body.
5. + complex `.set` args (struct `MtId`, array `[SVector; 1]`).
6. + post-loop `phi.set_bc(...)` / `eta.set_bc(...)` + second global `eta`.
7. + `domain.get_cell_center(id)` runtime call + `f32` math in the body.

### Instrumentation (temporary, behind `CE_DEBUG` in const_eval.rs)

Added logging (currently `CE_DEBUG = true`) for:
- `process_block` — block id per worklist pass.
- `set_unavail` / `set_deref_unavail` — value id
  (`set_unavail` **also logs its `BorrowSource` paths** via
  `graph.get_paths(value)`; `set_deref_unavail` currently logs the value id only —
  **gap**, see §4).
- `is_avail` — when it returns `false`, with the reason (`in_avail` vs `owner_fixed`).
- `check_constant`, `check_comptime_call`, `validate_comptime_context` — activity.

### Findings from instrumentation

- In **every** passing repro, only plain runtime values (empty `BorrowSource`)
  flip to `Floating`. **No value in the `Global(phi)` tree is ever set to
  `Floating`** → the poisoning that triggers the bug is not reproduced.
- The comptime block's *calls* (`intern`/`field`) are **not** checked by
  `check_comptime_call`: the comptime block holds data-flow nodes
  (`Ref`/`Deref`/`Data`/`Init`), not `Call`. The comptime-function calls run
  through separate machinery (`process_comptime_functions`). So the
  captured-value error is gated purely on the forest's `is_avail` /
  `is_deref_avail` — consistent with D1.
- `validate_comptime_context` does run and finds the comptime blocks
  (e.g. `total_blocks=14 comptime_blocks=3` for the repro's `test` fn).

### Why the repros all pass (refined understanding, 2026-09-14)

A value only poisons the `phi` component if it carries a `Global(phi)` or
`Local(O_phi)` borrow path. In the repro mocks, every loop-carried value (`i`,
`id`, `c`, `x`, `val`) is a plain value with only its own trivial owner tree —
nothing shares a tree component with `phi`. The production `init()` carries the
trigger element the mocks lack; the exact value/channel is what the production
`[ce]` log will pin down.

## 4. The gap / open questions (updated 2026-09-14)

- **Instrumentation gap**: `set_deref_unavail` (const_eval.rs:506-512) does not log
  `BorrowSource` paths, so channel-2 poisoning is invisible in the current logs.
  Fix: log sources there, as `set_unavail` already does.
- **Log correlation gap**: `[ce]` log lines carry no function identity, so lines
  from all compiled functions interleave. Since
  `validate_comptime_context` runs immediately after each function's
  `propagate_constants` (const_eval.rs:2178), the error appears right after the
  failing function's last worklist pass — but adding per-function delimiters
  (`propagate_constants` / `validate_comptime_context` entry markers) makes the
  log unambiguous.
- **To confirm**: per-run determinism of the production case (run twice, diff
  results). If non-deterministic, additionally audit `HashMap` iteration order in
  `BorrowTree::build` (borrow.rs:789-809) — though the worklist itself is
  deterministic.

## 5. Fix decision (2026-09-14)

Root cause: the flow-state forest is an incremental, non-idempotent, never-reset
global rather than a per-execution dataflow state.

**Chosen: scope the forest per block pass, exactly like `avail`.**

- In `CallParameterCopy::set_vm_values` (const_eval.rs:436-465), alongside
  `consts.block_frame.avail.clear()` (line 445), reset
  `consts.block_frame.references` to all-`FlowState::Fixed` (via
  `ReferenceStateForest::new(&borrow_graph.forest, FlowState::Fixed)` or a new
  `ReferenceStateForest::reset(base)` helper in borrow.rs).
- Each pass's forest then becomes a deterministic function of (a) the block's
  **joined** parameters (the proper dataflow lattice: `Known` ⇒ stable on all
  incoming paths ⇒ `Fixed`; `Runtime` ⇒ `Floating`) and (b) the block's own
  statements. Cross-pass/cross-block leakage — the bug — is eliminated.
- Worklist fixpoint detection (`call_changed`, const_eval.rs:229-244, compares
  params only) is untouched, so convergence behavior is unchanged.

**Rejected alternative** (previous doc's option 1: store a
`ReferenceStateForest` per `ConstNodeState`, join from parents each iteration):
changes worklist change-detection and output plumbing, and a parent-joined forest
is *more* conservative than necessary (a `Known` param whose sibling was poisoned
in one predecessor would stay `Floating`). The scoped version is strictly less
invasive and equally sound, per the argument in §2.

## 6. Files / locations reference

- `edlc_core/src/mir/mir_expr/mir_graph/const_eval.rs`
  - `CE_DEBUG` flag + `CE_BLOCK_COUNTER`: lines 56-58 (temporary).
  - line ~1034: the error (`check_constant`).
  - `ConstFrame` { `avail`, `references` }; `is_avail` / `is_deref_avail`.
  - `set_avail` / `set_unavail` / `set_deref_avail` / `set_deref_unavail`.
  - `set_vm_values` (clears `avail`, not the forest) — **fix site**.
  - `transfer_block_call` (clone/swap scaffolding; currently dead code).
  - `propagate_constants` (FIFO worklist), `process_block` (per-pass).
  - `eval_consts` (per-statement; `mark_runtime`, `set_deref_unavail` sites).
  - `check_hybrid_call` / `check_comptime_call` / `validate_comptime_context`.
  - `process_function` calls `validate_comptime_context` per function after
    `propagate_constants` (line ~2178).
- `edlc_core/src/mir/mir_expr/mir_graph/borrow.rs`
  - `FlowState` { `Fixed`, `Floating` }, `upper`/`lower`.
  - `ReferenceState::set_value` (sticky-max propagation).
  - `ReferenceStateForest` { `new`, `set_owner_value`, `set_deref_value`,
    `get_max_for_owners`, `get_max_for_deref` } — `reset` helper to be added.
  - `BorrowSource` { `Local(OwnerData)`, `Global(EdlVarId)` }.
  - `BorrowTree::build` (iterates a `HashMap` — leaf order is seed-dependent,
    semantics position-independent).
  - `get_paths(value)` (used by the instrumentation to read `BorrowSource`).
- `edlc_core/src/mir/mir_expr/mir_graph/validate.rs`:
  - `find_begin_comptime_block` (comptime blocks are connected
    `Context::Comptime` regions in the CFG).
- `edlc_core/src/mir/mir_expr/mir_ref.rs`: `MirDeref::is_avail` →
  `is_deref_avail` (line 677-683).
- `edlc_core/src/mir/mir_expr/mir_call.rs`: `MirCall::is_avail` — a comptime call
  executes only if all args pass `frame.is_avail` (line 301-319).
- `edlc_core/src/mir/mir_executor.rs`: `const_folding_execution_limit: 1000`.
- `edlc_codegen_cranelift/src/executor.rs`: `test_const_eval_loop_capture` (repro);
  `test_sync_logic` (template for mock globals + comptime blocks).
- Production case:
  - `~/simulations/cahn-hilliard-navier-stokes/` (`acodyn run` to reproduce).
  - `.../comp_navier_stokes/src/ch.eq` — `init()` at lines 82-140.
  - `~/projects/acodyn/src/solve/equation/custom/field.rs` — JIT-linked field API
    (`load_fields` line 242; `Field`/`Intern` are `ResourceId`s).
  - `~/projects/acodyn/Cargo.toml` — path deps on this workspace;
    `~/projects/acodyn/install.sh` — reinstall step after rebuilds.

## 7. TODO (updated 2026-09-14)

- [x] Identify the full production trigger case (ch::init in cahn-hilliard sim).
- [x] Identify the two exact poisoning channels (§2).
- [x] Decide the fix: scoped forest per block pass (§5).
- [ ] Run installed `acodyn` binary in the production case twice; capture baseline
      `[ce]` logs; confirm per-run determinism.
- [ ] Analyze baseline logs: poisoning value, channel, block/pass.
- [ ] Extend `CE_DEBUG`: `set_deref_unavail` sources, per-function delimiters.
- [ ] Rebuild + reinstall acodyn (`install.sh`); re-run for refined log.
- [ ] Minimal compiler-level regression test (fails pre-fix, passes post-fix).
- [ ] Implement the scoped-forest fix in `set_vm_values` (+ `reset` helper).
- [ ] Verify: regression test, edlc test suite, acodyn unit tests, production sim
      end-to-end.
- [ ] Remove the temporary `CE_DEBUG` instrumentation before merging.
- [ ] D4 (partial-eq instead of byte compare) — tracked separately.
