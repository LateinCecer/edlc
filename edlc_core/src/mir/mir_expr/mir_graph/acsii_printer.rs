/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

use std::io::Write;
use crate::mir::mir_expr::{BlockCall, MirBlockRef, MirDeref, MirDowncastRef, MirExprVariant, MirFlowGraph, MirPrinter, MirRef, MirValue};
use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::{Block, Seal, Statement};
use crate::mir::mir_expr::mir_graph::sync::SyncEvent;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_ref::RefOffset;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_type::MirTypeId;

/// Is a MIR printer implementation that prints the call graph in ASCII formatting.
pub struct AsciPrinter<'writer, W: Write> {
    writer: &'writer mut W,
}

impl<'writer, W: Write> AsciPrinter<'writer, W> {
    pub fn new(writer: &'writer mut W) -> Self {
        AsciPrinter { writer }
    }

    fn write_value(&mut self, val: &MirValue) -> Result<(), std::io::Error> {
        write!(self.writer, "${:x}", val.0)
    }

    fn write_type(&mut self, ty: &MirTypeId) -> Result<(), std::io::Error> {
        write!(self.writer, "{}", ty)
    }

    fn write_as(&mut self, el: &MirAs) -> Result<(), std::io::Error> {
        self.write_value(&el.val)?;
        write!(self.writer, " as ")?;
        self.write_type(&el.ty)
    }

    fn write_assign(&mut self, expr: &MirAssign) -> Result<(), std::io::Error> {
        write!(self.writer, "assign *")?;
        self.write_value(&expr.lhs)?;
        write!(self.writer, " = ")?;
        self.write_value(&expr.rhs)
    }

    fn write_call(&mut self, expr: &MirCall) -> Result<(), std::io::Error> {
        write!(self.writer, "call {:?} (", &expr.func)?;
        let mut first = true;
        for params in expr.args.iter() {
            if first {
                first = false;
            } else {
                write!(self.writer, ", ")?;
            }
            self.write_value(params)?;
        }
        for param in expr.comptime_args.iter() {
            if first {
                first = false;
            } else {
                write!(self.writer, ", ")?;
            }
            self.write_value(&param.value_expr)?;
        }
        write!(self.writer, ")")
    }

    fn write_constant(&mut self, expr: &MirConstant) -> Result<(), std::io::Error> {
        write!(self.writer, "const {}", expr.value)
    }

    fn write_data(&mut self, expr: &MirData) -> Result<(), std::io::Error> {
        write!(self.writer, "data ")?;
        write!(self.writer, " [")?;

        let mut first = true;
        for byte in expr.value.iter() {
            if first {
                first = false;
            } else {
                write!(self.writer, " ")?;
            }
            write!(self.writer, "{:02x}", byte)?;
        }
        write!(self.writer, "]")
    }

    fn write_literal(&mut self, expr: &MirLiteral) -> Result<(), std::io::Error> {
        write!(self.writer, "lit {:?}", expr.value)
    }

    fn write_ref(&mut self, expr: &MirRef) -> Result<(), std::io::Error> {
        if expr.mutable {
            write!(self.writer, "mut ")?;
        }
        write!(self.writer, "ref ")?;
        self.write_value(&expr.value)?;
        match &expr.offset {
            RefOffset::Entire => {
                write!(self.writer, " offset: none")?;
            }
            RefOffset::Const(offset) => {
                write!(self.writer, " offset: [{}..{}]", offset.offset, offset.offset + offset.size)?;
            }
            RefOffset::ArrayIndex { index, array_size, .. } => {
                write!(self.writer, " index: [")?;
                self.write_value(index)?;
                write!(self.writer, " < {array_size}]")?;
            }
            RefOffset::SliceIndex { index, slice_size, .. } => {
                write!(self.writer, " index: [")?;
                self.write_value(index)?;
                write!(self.writer, " < ")?;
                self.write_value(slice_size)?;
                write!(self.writer, "]")?;
            }
            RefOffset::ArrayRange { start, end, array_size, .. } => {
                write!(self.writer, " range: [")?;
                self.write_value(start)?;
                write!(self.writer, "..")?;
                self.write_value(end)?;
                write!(self.writer, " < {array_size}]")?;
            }
            RefOffset::SliceRange { start, end, slice_size, .. } => {
                write!(self.writer, " range: [")?;
                self.write_value(start)?;
                write!(self.writer, "..")?;
                self.write_value(end)?;
                write!(self.writer, " < ")?;
                self.write_value(slice_size)?;
                write!(self.writer, "]")?;
            }
        }
        Ok(())
    }

    fn write_deref(&mut self, expr: &MirDeref) -> Result<(), std::io::Error> {
        write!(self.writer, "*")?;
        self.write_value(&expr.value)
    }

    fn write_downcast(&mut self, expr: &MirDowncastRef) -> Result<(), std::io::Error> {
        write!(self.writer, "*ref")?;
        self.write_value(&expr.value)
    }

    fn write_array_init(&mut self, expr: &MirArrayInit) -> Result<(), std::io::Error> {
        write!(self.writer, "init [")?;
        match &expr.elements {
            MirArrayInitVariant::List(list) => {
                let mut first = true;
                for value in list.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(self.writer, ", ")?;
                    }
                    self.write_value(value)?;
                }
            }
            MirArrayInitVariant::Copy { val, len } => {
                self.write_value(val)?;
                write!(self.writer, "; {len:?}")?;
            }
        }
        write!(self.writer, "]")
    }

    fn write_type_init(&mut self, expr: &MirTypeInit) -> Result<(), std::io::Error> {
        write!(self.writer, "init ")?;
        let mut first = true;
        for value in expr.inits.iter() {
            if first {
                first = false;
            } else {
                write!(self.writer, ", ")?;
            }
            self.write_value(&value.val)?;
        }
        Ok(())
    }

    fn write_variable(&mut self, expr: &MirGlobalVar) -> Result<(), std::io::Error> {
        write!(self.writer, "global ref {:?}", expr.var)
    }

    fn write_event(&mut self, event: &SyncEvent) -> Result<(), std::io::Error> {
        write!(self.writer, "event {:x}", event.internal_value.0)
    }

    fn write_statement(&mut self, stat: &Statement, graph: &MirFlowGraph) -> Result<(), std::io::Error> {
        match stat {
            Statement::VarDef { var, value, .. } => {
                self.write_value(var)?;
                self.writer.write_all(b": ")?;
                let ty = graph.get_var_type(var);
                self.write_type(ty)?;
                self.writer.write_all(b" = ")?;

                match &value.ty {
                    MirExprVariant::ArrayInit => {
                        self.write_array_init(&graph.expressions.array_inits[value.id])
                    }
                    MirExprVariant::As => {
                        self.write_as(&graph.expressions.ases[value.id])
                    }
                    MirExprVariant::Call => {
                        self.write_call(&graph.expressions.call[value.id])
                    }
                    MirExprVariant::Literal => {
                        self.write_literal(&graph.expressions.literals[value.id])
                    }
                    MirExprVariant::Variable => {
                        self.write_variable(&graph.expressions.variables[value.id])
                    }
                    MirExprVariant::Constant => {
                        self.write_constant(&graph.expressions.constants[value.id])
                    }
                    MirExprVariant::Assign => {
                        self.write_assign(&graph.expressions.assigns[value.id])
                    }
                    MirExprVariant::Data => {
                        self.write_data(&graph.expressions.data[value.id])
                    }
                    MirExprVariant::Init => {
                        self.write_type_init(&graph.expressions.type_inits[value.id])
                    }
                    MirExprVariant::Ref => {
                        self.write_ref(&graph.expressions.refs[value.id])
                    }
                    MirExprVariant::Deref => {
                        self.write_deref(&graph.expressions.derefs[value.id])
                    }
                    MirExprVariant::DowncastRef => {
                        self.write_downcast(&graph.expressions.downcasts[value.id])
                    }
                }
            }
            Statement::VarMove { var, value, .. } => {
                self.write_value(var)?;
                self.writer.write_all(b": ")?;
                let ty = graph.get_var_type(var);
                self.write_type(ty)?;
                write!(self.writer, " = move ")?;
                self.write_value(value)
            }
            Statement::VarCopy { var, value, .. } => {
                self.write_value(var)?;
                self.writer.write_all(b": ")?;
                let ty = graph.get_var_type(var);
                self.write_type(ty)?;
                write!(self.writer, " = copy ")?;
                self.write_value(value)
            }
            Statement::Drop { value, .. } => {
                write!(self.writer, "drop ")?;
                self.write_value(value)
            }
            Statement::Sync { event, .. } => {
                write!(self.writer, "sync ")?;
                self.write_event(event)
            }
            Statement::Record { event, .. } => {
                write!(self.writer, "record ")?;
                self.write_event(event)
            }
        }
    }

    fn write_block_call(&mut self, call: &BlockCall) -> Result<(), std::io::Error> {
        self.write_block_id(&call.target)?;
        write!(self.writer, "(")?;
        let mut first = true;
        for param in call.params.iter() {
            if first {
                first = false;
            } else {
                write!(self.writer, ", ")?;
            }
            self.write_value(param)?;
        }
        write!(self.writer, ")")
    }

    fn write_seal(&mut self, seal: &Seal, _graph: &MirFlowGraph) -> Result<(), std::io::Error> {
        match seal {
            Seal::Jump(target, _debug) => {
                write!(self.writer, "jump ")?;
                self.write_block_call(target)
            }
            Seal::Return(value, _debug) => {
                write!(self.writer, "return ")?;
                self.write_value(value)
            }
            Seal::Cond { cond, then_target, else_target, debug: _ } => {
                write!(self.writer, "bif ")?;
                self.write_value(cond)?;
                write!(self.writer, " then ")?;
                self.write_block_call(then_target)?;
                write!(self.writer, " else ")?;
                self.write_block_call(else_target)
            }
            Seal::Switch { cond: _, targets: _, default: _, debug: _ } => {
                todo!()
            }
            Seal::Panic(value, _debug) => {
                write!(self.writer, "panic ")?;
                self.write_value(value)
            }
            Seal::None => {
                write!(self.writer, "WARNING: UNSEALED")
            }
        }
    }

    fn write_block_id(&mut self, id: &MirBlockRef) -> Result<(), std::io::Error> {
        write!(self.writer, "@{:x}", id.0)
    }

    fn write_block(&mut self, block: &Block, block_id: &MirBlockRef, graph: &MirFlowGraph) -> Result<(), std::io::Error> {
        write!(self.writer, "block ")?;
        self.write_block_id(block_id)?;
        write!(self.writer, " (")?;
        let mut first = true;
        for param in block.parameters.iter() {
            if first {
                first = false;
            } else {
                write!(self.writer, ", ")?;
            }
            self.write_value(param)?;
            write!(self.writer, ": ")?;
            let ty = graph.get_var_type(param);
            self.write_type(ty)?;
        }
        self.writer.write_all(b") [")?;
        write!(self.writer, "{}", block.ctx)?;
        self.writer.write_all(b"]:\n")?;

        // write body
        const SPACER: &[u8] = b"    ";
        for (line, statement) in block.statements.iter().enumerate() {
            write!(self.writer, " {:>3} |", line)?;
            self.writer.write_all(SPACER)?;
            self.write_statement(statement, graph)?;
            self.writer.write_all(b"\n")?;
        }
        write!(self.writer, " {:>3} |", block.statements.len())?;
        self.writer.write_all(SPACER)?;
        self.write_seal(&block.seal, graph)
    }
}

impl<'writer, W: Write> MirPrinter for AsciPrinter<'writer, W> {
    type Error = std::io::Error;

    fn print(&mut self, graph: &MirFlowGraph) -> Result<(), std::io::Error> {
        if graph.blocks.is_empty() {
            panic!("comptime function does not have any body!!!");
        }


        for (block_id, block) in graph.blocks.iter().enumerate() {
            let block_ref = MirBlockRef(block_id);
            self.write_block(block, &block_ref, graph)?;
            self.writer.write_all(b"\n\n\n")?;
        }
        Ok(())
    }
}
