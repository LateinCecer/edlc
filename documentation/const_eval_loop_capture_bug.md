# Const-eval: "non-constant captured in `comptime` block" — investigation

Status: **mechanism confirmed, minimal reproduction not yet triggered**
Last updated: 2026-09-11

## 1. The bug

A rare production error reported by the const-eval pipeline:

```
non-constant captured in `comptime` block
```

- Emitted from `check_constant`, `edlc_core/src/mir/mir_expr/mir_graph/const_eval.rs:997`
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

## 2. Root-cause hypothesis (D1) — CONFIRMED as the mechanism

The `references` flow-state forest is the culprit.

### Key facts (with locations)

- `ConstFrame.references: ReferenceStateForest<FlowState>` is a **single
  persistent field** on `ConstEval`.
  - Declared: `const_eval.rs` (`pub(crate) struct ConstFrame { avail, references }`).
  - Initialized **once** to all-`FlowState::Fixed` in `ConstEval::new`.
  - **Never reset** for the whole worklist run.
- `set_vm_values` (const_eval.rs) clears `block_frame.avail` on every block
  re-processing but **does not touch `references`**. So `avail` is fresh per block
  while the forest accumulates.
- For each block param: a `Known` param → `set_avail` (owner→`Fixed`); a
  `Runtime`/`Unknown` param → `set_unavail` (owner→**`Floating`**).
- `ReferenceState::set_value` (borrow.rs) is **sticky-max**: setting a node to
  `Floating` (a) overwrites the node and all its descendants to `Floating`, and
  (b) recomputes every ancestor as the **max of its children**
  (`FlowState::upper`, where `Floating > Fixed`). One `Floating` node therefore
  poisons the whole root→leaf chain it sits on.
- `is_avail(value)` (const_eval.rs) — the gate for whether a comptime call is
  executed — requires `avail.contains(value) && get_max_for_owners(value) == Fixed`.
  `is_deref_avail` is the same for the deref forest.

### The failure path

1. In some worklist iteration, a value that lives in the same borrow-tree
   component as the global `phi` becomes a `Runtime` block param.
2. `set_unavail` marks it `Floating`; max-propagation flips `phi`'s own node to
   `Floating`.
3. In a later iteration, `is_avail(phi)` is `false` → `phi.intern()` is not
   comptime-executed → the captured handle is `mark_runtime`'d.
4. `check_constant` on the captured value fails → **line-997 error**.

Aggravators specific to the snippet:
- The comptime block sits **inside the loop**, so it is re-executed on every
  back-edge re-pass — exactly when accumulated `Floating` states are present.
- The in-loop handle is fed into an `async` runtime call (`.set`), and the body
  carries runtime values (`val = std::rand::random(...)`, `i += 1`).

### Why D6 / D4 were ruled out

- **D6** (cross-function global mutation): globals are immutable — they cannot be
  written after declaration/initialization. Not applicable.
- **D4** (raw-byte `Known` comparison): real but not the trigger here; fixing it
  properly (partial-eq instead of byte compare) needs extra trait resolution and
  is a separate, larger effort.

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
- `set_unavail` / `set_deref_unavail` — value id **+ its `BorrowSource`**
  (via `graph.get_paths(value)`), to spot when a `Global(...)` tree is poisoned.
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

## 4. The gap / open questions

The trigger requires something that puts a `Global(phi)`-tree value into
`Floating`. Not yet isolated. Leading candidates:

1. A value **derived from `phi`** (the `Field` handle, or something from
   `intern()`/`field()`) that is a **block parameter** and becomes `Runtime` on
   some pass — the only path that calls `set_unavail` on it.
2. The `async`-ness of the returned handle interacting with the loop's back-edge
   re-passes.

Questions for the author (to pin the trigger):
- Is any `phi`-derived value ever passed **by `&mut`**, or used as a `comptime`
  parameter to another call?
- Does the trigger require the post-loop `set_bc` calls, or does it fire with
  just the loop?
- Roughly how many loop iterations / how large is `num_cells()` in the failing
  case? (Note: the worklist re-processes the body only a few times until the
  loop-carried counter joins to `Runtime`, so the 1000-iteration `force_runtime`
  cap is unlikely to be the trigger.)

Fastest paths forward:
- (a) A **reduced version of the production `init()`** (real types may be stubbed)
      that actually errors → point the instrumentation at the poisoning value.
- (b) Run the instrumented build against the real code and share the `[ce]` log —
      it will immediately show which value flips the `phi` tree to `Floating`.

## 5. Candidate fixes (once the repro is confirmed)

Root cause: the flow-state forest is an incremental, non-idempotent, never-reset
global rather than a proper dataflow value.

1. **Make the forest a real dataflow state** (preferred): store a
   `ReferenceStateForest` per `ConstNodeState`, join it from parents each
   iteration (using `FlowState::upper`), and seed each block's processing from
   that join instead of the sticky persistent field. (The clone/swap scaffolding
   in the dead `transfer_block_call`, const_eval.rs, shows this was intended.)
2. Lighter: at the start of each `process_block`, re-derive the forest from the
   joined parent state instead of inheriting the raw persistent one (scoping it
   per block like `avail`).

## 6. Files / locations reference

- `edlc_core/src/mir/mir_expr/mir_graph/const_eval.rs`
  - line 997: the error (`check_constant`).
  - `ConstFrame` { `avail`, `references` }; `is_avail` / `is_deref_avail`.
  - `set_avail` / `set_unavail` / `set_deref_avail` / `set_deref_unavail`.
  - `set_vm_values` (clears `avail`, not the forest).
  - `transfer_block_call` (clone/swap scaffolding; currently dead code).
  - `propagate_constants` (FIFO worklist), `process_block` (per-pass).
  - `eval_consts` (per-statement; `mark_runtime`, `set_deref_unavail` sites).
  - `check_hybrid_call` / `check_comptime_call` / `validate_comptime_context`.
- `edlc_core/src/mir/mir_expr/mir_graph/borrow.rs`
  - `FlowState` { `Fixed`, `Floating` }, `upper`/`lower`.
  - `ReferenceState::set_value` (sticky-max propagation).
  - `BorrowSource` { `Local(OwnerData)`, `Global(EdlVarId)` }.
  - `get_paths(value)` (used by the instrumentation to read `BorrowSource`).
- `edlc_core/src/mir/mir_executor.rs`: `const_folding_execution_limit: 1000`.
- `edlc_codegen_cranelift/src/executor.rs`: `test_const_eval_loop_capture` (repro);
  `test_sync_logic` (template for mock globals + comptime blocks).

## 7. TODO

- [ ] Obtain a reduced failing case (or `[ce]` log from the real code).
- [ ] Identify the exact value that flips the `phi` tree to `Floating`.
- [ ] Confirm minimal reproduction triggers line 997.
- [ ] Choose + implement the fix (dataflow-join forest vs per-block re-derivation).
- [ ] Remove the temporary `CE_DEBUG` instrumentation before merging.
