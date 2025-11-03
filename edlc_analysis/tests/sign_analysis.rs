use edlc_analysis::graph::{CfgNodeState, CfgNodeStateMut, CfgValueGenerator, CfgValueId, ConstraintLattice, GraphBuilder, HashNodeState, Lattice, LatticeElement, LogicSolver, NoopNode, PropagationWorkListForward, TransferFn};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum Sign {
    Positive,
    Negative,
    Zero,
    Unknown, // top
    #[default]
    Invalid, // bottom
}


#[derive(Clone, Debug)]
enum SignConflict {
    Mismatch(Sign, Sign),
}

impl Display for SignConflict {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SignConflict::Mismatch(a, b) => {
                write!(f, "sign mismatch: {a:?} does not equal {b:?}")
            }
        }
    }
}

impl Error for SignConflict {}

impl LatticeElement for Sign {
    type Conflict = SignConflict;

    fn lower(self, other: Self) -> Result<Self, Self::Conflict> {
        match (self, other) {
            (Sign::Invalid, _) | (Sign::Positive | Sign::Negative | Sign::Zero, Sign::Unknown) => Ok(self),
            (_, o) => Ok(o),
        }
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        match (self, other) {
            (Sign::Unknown, _) | (Sign::Positive | Sign::Negative | Sign::Zero, Sign::Invalid) => Ok(self),
            (_, o) => Ok(o),
        }
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (Sign::Invalid, _) | (Sign::Positive | Sign::Negative | Sign::Zero, Sign::Unknown) => true,
            (_, _) => false,
        }
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (Sign::Unknown, _) | (Sign::Positive | Sign::Negative | Sign::Zero, Sign::Invalid) => true,
            (_, _) => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
struct ExprId(usize);

impl Display for ExprId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Expr({:x})", self.0)
    }
}

#[derive(Default)]
struct ExprPool {
    pool: Vec<Expr>,
}

impl ExprPool {
    fn insert(&mut self, item: Expr) -> ExprId {
        let idx = self.pool.len();
        self.pool.push(item);
        ExprId(idx)
    }
}

impl Index<ExprId> for ExprPool {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self.pool[index.0]
    }
}

impl IndexMut<ExprId> for ExprPool {
    fn index_mut(&mut self, index: ExprId) -> &mut Self::Output {
        &mut self.pool[index.0]
    }
}


#[derive(Clone, Copy, Debug)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

enum Expr {
    Literal(i32),
    Var(CfgValueId),
    Input,
    Operator {
        op: Operator,
        lhs: ExprId,
        rhs: ExprId,
    },
}

impl Expr {
    /// Evaluates the sign of an expression.
    fn eval(&self, state: &HashNodeState<Sign>, pool: &ExprPool) -> Sign {
        match self {
            Expr::Literal(v) if *v > 0 => Sign::Positive,
            Expr::Literal(v) if *v < 0 => Sign::Negative,
            Expr::Literal(v) if *v == 0 => Sign::Zero,
            Expr::Literal(_) => Sign::Unknown,
            Expr::Var(id) => state.element_value(id),
            Expr::Input => Sign::Unknown,
            Expr::Operator { op: Operator::Add, lhs, rhs } => {
                let lhs = pool[*lhs].eval(state, pool);
                let rhs = pool[*rhs].eval(state, pool);

                match lhs {
                    Sign::Invalid => Sign::Invalid,
                    Sign::Zero => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Zero,
                        Sign::Negative => Sign::Negative,
                        Sign::Positive => Sign::Positive,
                        Sign::Unknown => Sign::Unknown,
                    },
                    Sign::Negative => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Negative,
                        Sign::Negative => Sign::Negative,
                        Sign::Positive => Sign::Unknown,
                        Sign::Unknown => Sign::Unknown,
                    },
                    Sign::Positive => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Positive,
                        Sign::Negative => Sign::Unknown,
                        Sign::Positive => Sign::Positive,
                        Sign::Unknown => Sign::Unknown,
                    },
                    Sign::Unknown => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        _ => Sign::Unknown,
                    }
                }
            }
            Expr::Operator { op: Operator::Sub, lhs, rhs } => {
                let lhs = pool[*lhs].eval(state, pool);
                let rhs = pool[*rhs].eval(state, pool);

                match lhs {
                    Sign::Invalid => Sign::Invalid,
                    Sign::Zero => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Zero,
                        Sign::Negative => Sign::Positive,
                        Sign::Positive => Sign::Negative,
                        Sign::Unknown => Sign::Unknown,
                    },
                    Sign::Negative => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Negative,
                        Sign::Negative => Sign::Unknown,
                        Sign::Positive => Sign::Negative,
                        Sign::Unknown => Sign::Unknown,
                    },
                    Sign::Positive => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Positive,
                        Sign::Negative => Sign::Positive,
                        Sign::Positive => Sign::Unknown,
                        Sign::Unknown => Sign::Unknown,
                    },
                    Sign::Unknown => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        _ => Sign::Unknown,
                    }
                }
            }
            Expr::Operator { op: Operator::Mul, lhs, rhs } => {
                let lhs = pool[*lhs].eval(state, pool);
                let rhs = pool[*rhs].eval(state, pool);

                match lhs {
                    Sign::Invalid => Sign::Invalid,
                    Sign::Zero => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        _ => Sign::Zero,
                    }
                    Sign::Negative => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Zero,
                        Sign::Negative => Sign::Positive,
                        Sign::Positive => Sign::Negative,
                        Sign::Unknown => Sign::Unknown,
                    }
                    Sign::Positive => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Zero,
                        Sign::Negative => Sign::Negative,
                        Sign::Positive => Sign::Positive,
                        Sign::Unknown => Sign::Unknown,
                    }
                    Sign::Unknown => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        _ => Sign::Unknown,
                    }
                }
            }
            Expr::Operator { op: Operator::Div, lhs, rhs } => {
                let lhs = pool[*lhs].eval(state, pool);
                let rhs = pool[*rhs].eval(state, pool);

                match lhs {
                    Sign::Invalid => Sign::Invalid,
                    Sign::Zero => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Invalid,
                        Sign::Negative => Sign::Zero,
                        Sign::Positive => Sign::Zero,
                        Sign::Unknown => Sign::Unknown,
                    }
                    Sign::Negative => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Invalid,
                        _ => Sign::Unknown,
                    }
                    Sign::Positive => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        Sign::Zero => Sign::Invalid,
                        _ => Sign::Unknown,
                    }
                    Sign::Unknown => match rhs {
                        Sign::Invalid => Sign::Invalid,
                        _ => Sign::Unknown,
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
enum SignTransferFn {
    Assign(CfgValueId, ExprId),
    Declare(CfgValueId),
    Other,
}

impl Display for SignTransferFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SignTransferFn::Assign(val, expr) => {
                write!(f, "f(v) = JOIN(v)[{val} -> eval(JOIN(v), {expr})]")
            }
            SignTransferFn::Declare(val) => {
                write!(f, "f(v) = JOIN(v)[{val} -> ┴]")
            }
            SignTransferFn::Other => {
                write!(f, "f(v) = JOIN(v)")
            }
        }
    }
}

impl TransferFn<ConstraintLattice<ExprPool, Self>, Sign> for SignTransferFn {
    fn transfer(&self, mut input: HashNodeState<Sign>, cfg: &ConstraintLattice<ExprPool, Self>) -> Result<HashNodeState<Sign>, SignConflict> {
        match self {
            SignTransferFn::Assign(id, expr_id) => {
                *input.element_value_mut(id) = cfg.state[*expr_id].eval(&input, &cfg.state);
            }
            SignTransferFn::Declare(id) => {
                *input.element_value_mut(id) = Sign::Invalid;
            }
            SignTransferFn::Other => (), // nothing to do here
        }
        Ok(input)
    }
}

impl NoopNode for SignTransferFn {
    fn noop() -> Self {
        SignTransferFn::Other
    }
}

#[derive(Debug)]
struct SignNode {
    name: String,
    fns: SignTransferFn,
}

#[test]
fn join() {
    assert_eq!(Sign::upper(Sign::Positive, Sign::Unknown).unwrap(), Sign::Unknown);
    assert_eq!(Sign::upper(Sign::Negative, Sign::Unknown).unwrap(), Sign::Unknown);
    assert_eq!(Sign::upper(Sign::Zero, Sign::Unknown).unwrap(), Sign::Unknown);
    assert_eq!(Sign::upper(Sign::Invalid, Sign::Unknown).unwrap(), Sign::Unknown);
    assert_eq!(Sign::upper(Sign::Unknown, Sign::Unknown).unwrap(), Sign::Unknown);
}

#[test]
fn solve_sign() {
    let mut graph_state = HashMap::default();

    let gen_state = CfgValueGenerator::default();
    let a = gen_state.generate();
    let b = gen_state.generate();
    let c = gen_state.generate();

    let mut cfg = ConstraintLattice {
        state: ExprPool::default(),
        lattice: Lattice::new(
            SignTransferFn::Other,
            SignTransferFn::Declare(a)
        ),
    };
    let v1 = cfg.lattice.top();
    let v2_1 = cfg.lattice.bottom();
    // built test CFG program
    let v2_2 = cfg.lattice.insert(
        &[],
        &[v2_1],
        SignTransferFn::Declare(b)
    );
    let v2_3 = cfg.lattice.insert(
        &[],
        &[v2_2],
        SignTransferFn::Declare(c)
    );
    let expr = cfg.state.insert(Expr::Literal(42));
    let v3 = cfg.lattice.insert(
        &[],
        &[v2_3],
        SignTransferFn::Assign(a, expr)
    );
    let expr = cfg.state.insert(Expr::Literal(87));
    let v4 = cfg.lattice.insert(
        &[],
        &[v3],
        SignTransferFn::Assign(b, expr)
    );
    let v5 = cfg.lattice.insert(
        &[],
        &[v4],
        SignTransferFn::Other
    );
    let val_a = cfg.state.insert(Expr::Var(a));
    let val_b = cfg.state.insert(Expr::Var(b));
    let expr = cfg.state.insert(Expr::Operator { op: Operator::Add, lhs: val_a, rhs: val_b });
    let v6 = cfg.lattice.insert(
        &[],
        &[v5],
        SignTransferFn::Assign(c, expr)
    );
    let v8 = cfg.lattice.insert(
        &[],
        &[v6],
        SignTransferFn::Other
    );
    let val_a = cfg.state.insert(Expr::Var(a));
    let val_b = cfg.state.insert(Expr::Var(b));
    let expr = cfg.state.insert(Expr::Operator { op: Operator::Sub, lhs: val_a, rhs: val_b });
    let v7 = cfg.lattice.insert(
        &[v8],
        &[v5],
        SignTransferFn::Assign(c, expr)
    );

    println!("done building CFG test program.");
    // worklist solver
    PropagationWorkListForward.solve(&cfg, &mut graph_state).unwrap();
    println!("done analysing program.");

    // print info
    println!(" --- SIGN ANALYSIS OUTPUT --- ");
    let states = [v1, v2_1, v2_2, v2_3, v3, v4, v5, v6, v7, v8];
    for state in states {
        let node_name = format!("{state}");
        let node_state = &graph_state.get(&state);
        println!("  ° <{node_name:<8}>");
        if let Some(node_state) = *node_state {
            println!("    {node_state:?}");
        }
        println!();
    }
    println!(" --- SIGN ANALYSIS OUTPUT --- ");
}

#[test]
fn solve_sign_builder() {
    let gen_state = CfgValueGenerator::default();
    let a = gen_state.generate();
    let b = gen_state.generate();
    let c = gen_state.generate();

    let mut state = ExprPool::default();
    let mut builder: GraphBuilder<SignTransferFn> = GraphBuilder::default();

    let entry_block = builder.create_block();
    let if_block = builder.create_block();
    let else_block = builder.create_block();
    let merge_block = builder.create_block();

    builder.view_mut(entry_block).ins(SignTransferFn::Declare(a));
    builder.view_mut(entry_block).ins(SignTransferFn::Declare(b));
    builder.view_mut(entry_block).ins(SignTransferFn::Declare(c));
    builder.view_mut(entry_block).ins(SignTransferFn::Assign(a, state.insert(Expr::Literal(42))));
    builder.view_mut(entry_block).ins(SignTransferFn::Assign(b, state.insert(Expr::Literal(87))));
    builder.view_mut(entry_block).ins(SignTransferFn::Other);
    builder.view_mut(entry_block).ins_cond(if_block);
    builder.view_mut(entry_block).ins_jump(else_block);

    let val_a = state.insert(Expr::Var(a));
    let val_b = state.insert(Expr::Var(b));
    builder.view_mut(if_block).ins(SignTransferFn::Assign(c, state.insert(Expr::Operator { op: Operator::Add, lhs: val_a, rhs: val_b })));
    builder.view_mut(if_block).ins_jump(merge_block);

    let val_a = state.insert(Expr::Var(a));
    let val_b = state.insert(Expr::Var(b));
    builder.view_mut(else_block).ins(SignTransferFn::Assign(c, state.insert(Expr::Operator { op: Operator::Sub, lhs: val_a, rhs: val_b })));
    builder.view_mut(else_block).ins_jump(merge_block);

    builder.view_mut(merge_block).ins(SignTransferFn::Other);

    let cfg = ConstraintLattice {
        state,
        lattice: builder.make(),
    };

    println!("done building CFG test program.");
    println!("{}", cfg.lattice);

    // worklist solver
    let mut graph_state = HashMap::default();
    PropagationWorkListForward.solve(&cfg, &mut graph_state).unwrap();
    println!("done analysing program.");

    // print info
    println!(" --- SIGN ANALYSIS OUTPUT --- ");
    let mut keys = graph_state.keys().collect::<Vec<_>>();
    keys.sort();
    for state in keys {
        let node_name = format!("{state}");
        let node_state = &graph_state.get(state);
        println!("  ° <{node_name:<8}>");
        if let Some(node_state) = *node_state {
            println!("    {node_state:?}");
        }
        println!();
    }
    println!(" --- SIGN ANALYSIS OUTPUT --- ");
}
