use std::ops::Index;
use cranelift_codegen::ir;
use cranelift_codegen::ir::InstBuilder;
use crate::codegen::FunctionTranslator;
use crate::compiler::JIT;
use edlc_core::prelude::mir_expr::{Block, BlockCall, MirBlockRef, MirFlowGraph, Seal, Statement};
use edlc_core::prelude::{MirError, MirPhase};
use edlc_core::prelude::index_map::IndexMap;
use crate::layout::SSARepr;

/// Maps MIR blocks to Cranelift blocks.
struct BlockMapper {
    mapping: IndexMap<ir::Block>,
}

impl Index<MirBlockRef> for BlockMapper {
    type Output = ir::Block;

    fn index(&self, index: MirBlockRef) -> &Self::Output {
        &self.mapping[index.ordinal()]
    }
}

fn cfg_codegen<Runtime>(
    cfg: &MirFlowGraph,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
) -> Result<(), MirError<JIT<Runtime>>> {
    let mut mapping = BlockMapper {
        mapping: IndexMap::default(),
    };
    // insert blocks and block parameters for those blocks
    for block_ref in cfg.blocks() {
        let block = cfg.get_block(&block_ref).unwrap();
        let ir_block = builder.builder.create_block();
        builder.builder.switch_to_block(ir_block);

        if block_ref == cfg.root() {
            // insert block parameters from function definition
            builder.builder.append_block_params_for_function_params(ir_block);
            let ir_params = builder.builder.block_params(ir_block);

            let mut i = 0usize;
            for param in block.parameters.iter() {


            }
        } else {
            // insert block parameters
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

        mapping.mapping.view_mut(block_ref.ordinal()).set(ir_block);
    }

    // translate blocks to cranelift IR
    for block_ref in cfg.blocks() {
        let block = cfg.get_block(&block_ref).unwrap();
        block_codegen(&block_ref, block, builder, phase, &mapping)?;
    }
    builder.builder.seal_all_blocks();
    Ok(())
}

fn block_codegen<Runtime>(
    block_ref: &MirBlockRef,
    block: &Block,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
    mapping: &BlockMapper,
) -> Result<(), MirError<JIT<Runtime>>> {
    let current_block = mapping[*block_ref];
    builder.builder.switch_to_block(current_block);
    for statement in block.statements.iter() {
        statement_codegen(statement, builder, phase)?;
    }
    seal_codegen(&block.seal, builder, phase, mapping)
}

fn statement_codegen<Runtime>(
    statement: &Statement,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
) -> Result<(), MirError<JIT<Runtime>>> {
    Ok(())
}

fn seal_codegen<Runtime>(
    seal: &Seal,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
    mapping: &BlockMapper,
) -> Result<(), MirError<JIT<Runtime>>> {
    match seal {
        Seal::Jump(call, _) => {

        }
        Seal::Return(value, _) => {

        }
        Seal::Panic(value, _) => {

        }
        Seal::Cond { cond, then_target, else_target, .. } => {

        }
        Seal::Switch { cond, targets, default, .. } => {

        }
        Seal::None => unreachable!()
    }
    Ok(())
}

fn block_call_codegen<Runtime>(
    call: &BlockCall,
    builder: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
    mapping: &BlockMapper,
) -> Result<(), MirError<JIT<Runtime>>> {
    let ir_target = mapping[call.target];
    let params = call.params
        .iter()
        .filter_map(|param| if builder.layout.is_block_param_on_reg(param, &phase.types) {
            let value = builder.layout
                .load_pod(param, &builder.ir_values, &mut builder.builder, &phase.types)
                .expect("IR value for JUMP instruction parameters missing");
            Some(ir::BlockArg::Value(value))
        } else {
            None
        })
        .collect::<Vec<_>>();
    builder.builder.ins().jump(ir_target, params.iter());
    Ok(())
}
