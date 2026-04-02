use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::layout::SSARepr;
use cranelift_codegen::ir;
use cranelift_codegen::ir::{InstBuilder, JumpTableData};
use edlc_core::prelude::index_map::IndexMap;
use edlc_core::prelude::mir_expr::{Block, BlockCall, MirBlockRef, MirExprContainer, MirExprVariant, MirFlowGraph, Seal, Statement};
use edlc_core::prelude::mir_type::MirTypeId;
use edlc_core::prelude::{MirError, MirPhase};
use std::ops::Index;

/// Maps MIR blocks to Cranelift blocks.
struct BlockMapper {
    mapping: IndexMap<ir::Block>,
    return_bufs: Option<IndexMap<ir::Value>>,
    _eff_return_value: MirTypeId,
}

impl Index<MirBlockRef> for BlockMapper {
    type Output = ir::Block;

    fn index(&self, index: MirBlockRef) -> &Self::Output {
        &self.mapping[index.ordinal()]
    }
}

pub(crate) fn cfg_codegen<Runtime>(
    cfg: &MirFlowGraph,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
) -> Result<(), MirError<JIT<Runtime>>> {
    let mut mapping = BlockMapper {
        mapping: IndexMap::default(),
        return_bufs: if builder.function_layout.return_via_buffer() {
            Some(IndexMap::default())
        } else {
            None
        },
        _eff_return_value: builder.function_layout
            .return_type
            .unwrap_or_else(|| phase.types.usize()),
    };
    // insert blocks and block parameters for those blocks
    for block_ref in cfg.blocks() {
        let ir_block = builder.builder.create_block();
        mapping.mapping.view_mut(block_ref.ordinal()).set(ir_block);
    }

    // translate blocks to cranelift IR
    for block_ref in cfg.blocks() {
        prepare_block(cfg, &block_ref, builder, phase, &mut mapping)?;
        let block = cfg.get_block(&block_ref).unwrap();
        block_codegen(&block_ref, block, &cfg.expressions, builder, phase, &mapping)?;
    }
    builder.builder.seal_all_blocks();
    Ok(())
}

fn prepare_block<Runtime>(
    cfg: &MirFlowGraph,
    block_ref: &MirBlockRef,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
    mapping: &mut BlockMapper,
) -> Result<(), MirError<JIT<Runtime>>> {
    let ir_block = mapping[*block_ref];
    builder.builder.switch_to_block(ir_block);
    if block_ref == &cfg.root() {
        // insert block parameters from function definition
        builder.builder.append_block_params_for_function_params(ir_block);
        let ret_buf = builder.function_layout.map(
            ir_block,
            &builder.layout,
            &mut builder.ir_values,
            &mut builder.builder,
            &phase.types,
            &builder.abi,
        );
        if let Some(ret_bufs) = mapping.return_bufs.as_mut() {
            ret_bufs.view_mut(block_ref.ordinal()).set(ret_buf.unwrap());
        } else {
            assert!(ret_buf.is_none());
        }
    } else {
        // insert return buffer parameter if necessary
        if let Some(ret_bufs) = mapping.return_bufs.as_mut() {
            let (ir_ty, _) = SSARepr::itype_for_alignment(builder.abi.pointer_width);
            let ret_buf = builder.builder.append_block_param(ir_block, ir_ty);
            ret_bufs.view_mut(block_ref.ordinal()).set(ret_buf);
        }
        // insert block parameters
        let block = cfg.get_block(&block_ref).unwrap();
        for param in block.parameters.iter() {
            if !builder.layout.is_block_param_on_reg(param, &phase.types) {
                continue;
            }
            let ty = builder.layout.get_ty(param).unwrap();
            let ir_ty = SSARepr::pod(ty, &phase.types).unwrap();
            let value = builder.builder.append_block_param(ir_block, ir_ty);
            builder.layout.store_pod(
                value,
                param,
                &mut builder.ir_values,
                &mut builder.builder,
                &phase.types,
            );
        }
    }
    Ok(())
}

fn block_codegen<Runtime>(
    block_ref: &MirBlockRef,
    block: &Block,
    expressions: &MirExprContainer,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
    mapping: &BlockMapper,
) -> Result<(), MirError<JIT<Runtime>>> {
    for statement in block.statements.iter() {
        statement_codegen(expressions, statement, builder, phase)?;
    }
    seal_codegen(block_ref, &block.seal, builder, phase, mapping)
}

fn statement_codegen<Runtime>(
    expressions: &MirExprContainer,
    statement: &Statement,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
) -> Result<(), MirError<JIT<Runtime>>> {
    match statement {
        Statement::VarDef { var, value, .. } => {
            match value.ty {
                MirExprVariant::ArrayInit => {
                    expressions.get_array_init(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::As => {
                    expressions.get_as(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::Call => {
                    expressions.get_call(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::Literal => {
                    expressions.get_literal(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::Variable => {
                    expressions.get_variable(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::Constant => {
                    expressions.get_constant(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::Assign => {
                    expressions.get_assign(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::Data => {
                    expressions.get_data(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::Init => {
                    expressions.get_init(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::Ref => {
                    expressions.get_ref(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::Deref => {
                    expressions.get_deref(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
                MirExprVariant::DowncastRef => {
                    expressions.get_downcast_ref(*value).compile(
                        builder,
                        phase,
                        var,
                        value,
                    )
                }
            }
        }
        Statement::VarMove { var, value, .. } => {
            builder.layout.cpy(
                value,
                var,
                &mut builder.ir_values,
                &mut builder.builder,
                &phase.types,
                &builder.abi,
            );
            Ok(())
        }
        Statement::VarCopy { var, value, .. } => {
            builder.layout.cpy(
                value,
                var,
                &mut builder.ir_values,
                &mut builder.builder,
                &phase.types,
                &builder.abi,
            );
            Ok(())
        }
        Statement::Drop { .. } => {
            Ok(())
        }
        Statement::Sync { .. } => {
            Ok(())
        }
        Statement::Record { .. } => {
            Ok(())
        }
    }
}

fn seal_codegen<Runtime>(
    current_block: &MirBlockRef,
    seal: &Seal,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
    mapping: &BlockMapper,
) -> Result<(), MirError<JIT<Runtime>>> {
    match seal {
        Seal::Jump(call, _) => {
            let args = block_call_codegen(current_block, call, builder, phase, mapping)?;
            builder.builder.ins()
                .jump(mapping[call.target], args.iter());
        }
        Seal::Return(value, _) => {
            if let Some(ret_buf) = mapping.return_bufs.as_ref() {
                let ptr = ret_buf[current_block.ordinal()];
                builder.layout.write_raw_ptr(
                    value,
                    ptr,
                    0,
                    &mut builder.ir_values,
                    &mut builder.builder,
                    &phase.types,
                    &builder.abi,
                );
                builder.builder
                    .ins()
                    .return_(&[]);
            } else {
                let mut output = Vec::new();
                builder.layout.load_eightbytes(
                    value,
                    &builder.ir_values,
                    &mut builder.builder,
                    &phase.types,
                    &builder.abi,
                    &mut output,
                );
                builder.builder
                    .ins()
                    .return_(&output);
            }
        }
        Seal::Panic(_value, _) => {
            builder.panic("")?;
        }
        Seal::Cond { cond, then_target, else_target, .. } => {
            let args_then = block_call_codegen(
                current_block, then_target, builder, phase, mapping)?;
            let args_else = block_call_codegen(
                current_block, else_target, builder, phase, mapping)?;
            let cond = builder.layout.load_pod(
                cond, &builder.ir_values, &mut builder.builder, &phase.types).unwrap();
            builder.builder
                .ins()
                .brif(
                    cond,
                    mapping[then_target.target],
                    &args_then,
                    mapping[else_target.target],
                    &args_else,
                );
        }
        Seal::Switch { cond, targets, default, .. } => {
            let mut value_list = ir::ValueListPool::new();
            let default_args = block_call_codegen(
                current_block, default, builder, phase, mapping)?;
            let default_call = ir::BlockCall::new(
                mapping[default.target], default_args, &mut value_list);

            let mut target_calls = Vec::new();
            for target in targets.iter() {
                let target_args = block_call_codegen(
                    current_block,
                    &target.block_call,
                    builder,
                    phase,
                    mapping,
                )?;
                let target_call = ir::BlockCall::new(
                    mapping[target.block_call.target],
                    target_args,
                    &mut value_list,
                );
                target_calls.push(target_call);
            }

            let table_data = JumpTableData::new(default_call, &target_calls);
            let table = builder.builder.create_jump_table(table_data);
            let cond = builder.layout.load_pod(
                cond, &builder.ir_values, &mut builder.builder, &phase.types).unwrap();
            builder.builder
                .ins()
                .br_table(cond, table);
        }
        Seal::None => unreachable!()
    }
    Ok(())
}

fn block_call_codegen<Runtime>(
    current_block: &MirBlockRef,
    call: &BlockCall,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
    mapping: &BlockMapper,
) -> Result<Vec<ir::BlockArg>, MirError<JIT<Runtime>>> {
    let mut params = Vec::new();
    if let Some(ret_bufs) = mapping.return_bufs.as_ref() {
        params.push(ir::BlockArg::Value(ret_bufs[current_block.ordinal()]));
    }
    params.extend(call.params
        .iter()
        .filter_map(|param| if builder.layout.is_block_param_on_reg(param, &phase.types) {
            let value = builder.layout
                .load_pod(param, &builder.ir_values, &mut builder.builder, &phase.types)
                .expect("IR value for JUMP instruction parameters missing");
            Some(ir::BlockArg::Value(value))
        } else {
            None
        }));
    Ok(params)
}
