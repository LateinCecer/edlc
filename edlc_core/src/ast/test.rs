/*
 *    Copyright 2025 Adrian Paskert
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */


mod module;


use std::error::Error;
use log::{error, info};
use crate::ast::ast_expression::AstExpr;
use crate::ast::{AstModule, IntoHir};
use crate::ast::ast_param_env::{AstParamDef, AstParamEnv};
use crate::core::edl_fn::{EdlFnArgument, EdlFnParam, EdlFnRet, EdlFnSignature};
use crate::core::edl_param_env::{EdlParameterEnv, EdlGenericParam, EdlGenericParamVariant};
use crate::core::edl_type;
use crate::core::edl_type::{EdlMaybeType, EdlTypeRegistry};
use crate::core::edl_value::EdlConstValue;
use crate::hir::hir_expr::HirExpr;
use crate::hir::{HirPhase, IntoEdl, ResolveTypes};
use crate::parser::{Parsable, Parser};
use crate::resolver::{ItemInit, ItemSrc, TopLevelNameResolver};
use crate::{inline_code, setup_logger};
use crate::core::type_analysis::*;

fn test<T>(res: Result<T, impl Error>) -> T {
    match res {
        Ok(res) => res,
        Err(e) => {
            error!("{}", e);
            panic!();
        }
    }
}

fn print_errors(err: Vec<impl Error>) {
    for err in err.iter() {
        error!("Compiler Error: {}", err);
    }
    if !err.is_empty() {
        error!("Compiler failed");
    }
}

fn populate_test_resolver(
    resolver: &mut TopLevelNameResolver,
    type_reg: &mut EdlTypeRegistry,
) {
    resolver.push_module("core".to_string());
    // push base types manually
    resolver.push_raw_type("u8".to_string(), edl_type::EDL_U8).unwrap();
    resolver.push_raw_type("u16".to_string(), edl_type::EDL_U16).unwrap();
    resolver.push_raw_type("u32".to_string(), edl_type::EDL_U32).unwrap();
    resolver.push_raw_type("u64".to_string(), edl_type::EDL_U64).unwrap();
    resolver.push_raw_type("u128".to_string(), edl_type::EDL_U128).unwrap();
    resolver.push_raw_type("usize".to_string(), edl_type::EDL_USIZE).unwrap();

    resolver.push_raw_type("i8".to_string(), edl_type::EDL_I8).unwrap();
    resolver.push_raw_type("i16".to_string(), edl_type::EDL_I16).unwrap();
    resolver.push_raw_type("i32".to_string(), edl_type::EDL_I32).unwrap();
    resolver.push_raw_type("i64".to_string(), edl_type::EDL_I64).unwrap();
    resolver.push_raw_type("i128".to_string(), edl_type::EDL_I128).unwrap();
    resolver.push_raw_type("isize".to_string(), edl_type::EDL_ISIZE).unwrap();

    resolver.push_raw_type("f32".to_string(), edl_type::EDL_F32).unwrap();
    resolver.push_raw_type("f64".to_string(), edl_type::EDL_F64).unwrap();

    resolver.push_raw_type("bool".to_string(), edl_type::EDL_BOOL).unwrap();
    resolver.push_raw_type("char".to_string(), edl_type::EDL_CHAR).unwrap();
    resolver.push_raw_type("str".to_string(), edl_type::EDL_STR).unwrap();


    resolver.pop();
    resolver.push_module("std".to_string());
    let std_scope = *resolver.current_scope().unwrap();


    let mut params = EdlParameterEnv::default();
    params.params.push(EdlGenericParam {
        name: "T".to_string(),
        variant: EdlGenericParamVariant::Type,
    });
    let env = type_reg.insert_parameter_env(params);

    resolver.push_fn("add_42".to_string());
    resolver.push_top_level_item(
        "add_42".to_string(),
        ItemSrc::Intrinsic("".to_string()),
        ItemInit::Function {
            sig: EdlFnSignature {
                name: "add_42".to_string(),
                params: vec![
                    EdlFnParam {
                        name: "val".to_string(),
                        ty: type_reg.generic(env, 0),
                        mutable: false,
                        comptime: false,
                    }
                ],
                comptime: true,
                comptime_only: false,
                env,
                scope: *resolver.current_scope().unwrap(),
                ret: type_reg.generic(env, 0),
            },
            scope: *resolver.current_scope().unwrap(),
        },
        type_reg,
    ).unwrap();



    let mut params = EdlParameterEnv::default();
    params.params.push(EdlGenericParam {
        name: "N".to_string(),
        variant: EdlGenericParamVariant::Const(edl_type::EDL_USIZE),
    });
    params.params.push(EdlGenericParam {
        name: "T".to_string(),
        variant: EdlGenericParamVariant::Type,
    });
    resolver.push_top_level_item(
        "MyData".to_string(),
        ItemSrc::Intrinsic("@ty std".to_string()),
        ItemInit::Type {
            params,
        },
        type_reg,
    ).unwrap();


    let mut params = EdlParameterEnv::default();
    params.params.push(EdlGenericParam {
        name: "T".to_string(),
        variant: EdlGenericParamVariant::Type,
    });
    params.params.push(EdlGenericParam {
        name: "NSIZE".to_string(),
        variant: EdlGenericParamVariant::Const(edl_type::EDL_USIZE),
    });
    params.params.push(EdlGenericParam {
        name: "DIM".to_string(),
        variant: EdlGenericParamVariant::Const(edl_type::EDL_USIZE),
    });

    resolver.push_top_level_item(
        "Domain".to_string(),
        ItemSrc::Intrinsic("@ty std".to_string()),
        ItemInit::Type {
            params: params.clone(),
        },
        type_reg,
    ).unwrap();

    let env = type_reg.insert_parameter_env(params.clone());
    let var = resolver.find_top_level_type(&vec!["Domain".to_string()].into(), type_reg).unwrap();
    let mut domain_instance = type_reg.new_type_instance(var).unwrap();

    let tmp = type_reg.generic(env, 0);
    domain_instance.param.set_type(0, tmp).unwrap();
    domain_instance.param.insert_const(1, EdlConstValue::GenericConst {
        param: env,
        index: 1,
    }, type_reg).unwrap();
    domain_instance.param.insert_const(2, EdlConstValue::GenericConst {
        param: env,
        index: 2,
    }, type_reg).unwrap();

    resolver.push_fn("load_domain".to_string());
    let fn_scope = *resolver.current_scope().unwrap();
    resolver.pop();

    resolver.push_top_level_item(
        "load_domain".to_string(),
        ItemSrc::Intrinsic("@fn std".to_string()),
        ItemInit::Function {
            sig: EdlFnSignature {
                name: "".to_string(),
                env,
                scope: *resolver.current_scope().unwrap(),
                comptime: true,
                comptime_only: false,
                ret: domain_instance,
                params: vec![],
            },
            scope: fn_scope,
        },
        type_reg,
    ).unwrap();

    resolver.push_fn("load_num_faces".to_string());
    let fn_scope = *resolver.current_scope().unwrap();
    resolver.pop();
    resolver.push_top_level_item(
        "load_num_faces".to_string(),
        ItemSrc::Intrinsic("@fn std".to_string()),
        ItemInit::Function {
            sig: EdlFnSignature {
                name: "".to_string(),
                env: type_reg.new_env(),
                scope: *resolver.current_scope().unwrap(),
                comptime: true,
                comptime_only: false,
                ret: type_reg.usize(),
                params: vec![],
            },
            scope: fn_scope,
        },
        type_reg,
    ).unwrap();

    resolver.push_fn("load_spatial_dims".to_string());
    let fn_scope = *resolver.current_scope().unwrap();
    resolver.pop();
    resolver.push_top_level_item(
        "load_spatial_dims".to_string(),
        ItemSrc::Intrinsic("@fn std".to_string()),
        ItemInit::Function {
            sig: EdlFnSignature {
                name: "".to_string(),
                env: type_reg.new_env(),
                scope: *resolver.current_scope().unwrap(),
                comptime: true,
                comptime_only: false,
                ret: type_reg.usize(),
                params: vec![
                    EdlFnParam {
                        name: "config_file".to_string(),
                        mutable: false,
                        comptime: false,
                        ty: type_reg.str(),
                    },
                    EdlFnParam {
                        name: "index".to_string(),
                        mutable: false,
                        comptime: false,
                        ty: type_reg.usize(),
                    }
                ],
            },
            scope: fn_scope,
        },
        type_reg,
    ).unwrap();


    resolver.push_module("fields".to_string());
    let mut params = EdlParameterEnv::default();
    params.params.push(EdlGenericParam {
        name: "NSIZE".to_string(),
        variant: EdlGenericParamVariant::Const(edl_type::EDL_USIZE),
    });
    params.params.push(EdlGenericParam {
        name: "DIM".to_string(),
        variant: EdlGenericParamVariant::Const(edl_type::EDL_USIZE),
    });
    let scope = resolver.push_top_level_item(
        "BoundaryField".to_string(),
        ItemSrc::Intrinsic("@ty std::fields".to_string()),
        ItemInit::Type {
            params: params.clone(),
        },
        type_reg
    ).unwrap();


    resolver.revert_to_scope(&scope);
    resolver.push_fn("vector".to_string());
    let fn_scope = *resolver.current_scope().unwrap();
    resolver.pop();

    resolver.push_top_level_item(
        "vector".to_string(),
        ItemSrc::Intrinsic("@fn std::fields::BoundaryField".to_string()),
        ItemInit::Function {
            sig: EdlFnSignature {
                name: "".to_string(),
                env: type_reg.insert_parameter_env(params.clone()),
                scope: *resolver.current_scope().unwrap(),
                comptime: false,
                comptime_only: false,
                ret: type_reg.empty(),
                params: vec![],
            },
            scope: fn_scope
        },
        type_reg,
    ).unwrap();

    resolver.push_fn("scalar".to_string());
    let fn_scope = *resolver.current_scope().unwrap();
    resolver.pop();
    resolver.push_top_level_item(
        "scalar".to_string(),
        ItemSrc::Intrinsic("@fn std::fields::BoundaryField".to_string()),
        ItemInit::Function {
            sig: EdlFnSignature {
                name: "".to_string(),
                env: type_reg.insert_parameter_env(params),
                scope: *resolver.current_scope().unwrap(),
                comptime: false,
                comptime_only: false,
                ret: type_reg.empty(),
                params: vec![],
            },
            scope: fn_scope,
        },
        type_reg,
    ).unwrap();

    resolver.revert_to_scope(&std_scope);
    resolver.pop(); // return to empty stack
}

#[test]
fn test_parse_module() {
    let _ = setup_logger();
    let src = r#"
        mod test_module;

        submod child;
        submod momentum;
        submod pressure;
        submod density;

        use std::fields;

        const N: usize = load_num_faces("config.exp");
        const DIM: usize = load_spatial_dims("config.exp");

        let domain: std::Domain<N, DIM> = load_domain("config.exp");
        let velocity: fields::BoundaryField<2, DIM> = fields::BoundaryField::vector(domain);
        let pressure: _ = fields::BoundaryField::scalar(domain);
        let density: fields::BoundaryField::<1, 3> = fields::boundaryField::scalar(domain);

        fn correct_boundaries<const N: usize, const DIM: usize>(field: mut fields::BoundaryField::<N, DIM>) {
            use std::buffers;
            let tmp = test();

            buffers::reset_status_flag(field);
            buffers::update_boundary_buffers(field);
            buffers::update_boundary_gradients(field);
            buffers::update_gradients(field);
        }

        fn update_momentum() {}
        fn update_pressure() {}
        fn update_density() {}
        "#;

    let mut resolver = TopLevelNameResolver::default();
    let mut type_reg = EdlTypeRegistry::default();

    // register types
    populate_test_resolver(&mut resolver, &mut type_reg);
    resolver.push_module("test".to_string());

    let mut parser = Parser::with_env(src, &mut resolver, &mut type_reg, inline_code!(src));
    let module = AstModule::parse(&mut parser, vec!["test".to_string()].into(), String::new());
    let module = test(module);
    dbg!(module);
}

#[test]
fn test_parse_literal() {
    let _ = setup_logger();
    let src = r#"32"#;
    let module_src = inline_code!(src);

    let mut resolver = TopLevelNameResolver::default();
    let mut type_reg = EdlTypeRegistry::default();

    // register types
    populate_test_resolver(&mut resolver, &mut type_reg);
    resolver.push_module("test".to_string());

    let mut parser = Parser::with_env(src, &mut resolver, &mut type_reg, module_src);
    let ast_lit = test(AstExpr::parse(&mut parser));
    info!("AST lit = {:?}", ast_lit);

    let mut hir_phase = HirPhase::new(type_reg, resolver);
    let mut hir_lit = test(ast_lit.hir_repr(&mut hir_phase));
    info!("HIR lit = {:?}", hir_lit);

    // change the type if the literal through some means
    let mut target_ty = hir_phase.types.usize();
    let mut infer_state = InferState::new();
    let mut infer = hir_phase.infer_from(&mut infer_state);
    let type_uid = hir_lit.get_type_uid(&mut infer);
    let node = infer.state.node_gen.gen__();
    infer.at(node)
        .eq(&type_uid, &target_ty)
        .unwrap();
    hir_lit.finalize_types(&mut infer);

    let const_lit = test(hir_lit.as_const_value(&mut hir_phase));
    info!("EDL const = {:?}", const_lit);
}

#[test]
fn test_parse_param_def() {
    let _ = setup_logger();
    let src = r#"<32, core::f32>"#;
    let module_src = inline_code!(src);

    let mut resolver = TopLevelNameResolver::default();
    let mut type_reg = EdlTypeRegistry::default();

    // register types
    populate_test_resolver(&mut resolver, &mut type_reg);
    resolver.push_module("test".to_string());

    dbg!(resolver.find_top_level_type(&vec!["core".to_string(), "f32".to_string()].into(), &type_reg));

    let mut parser = Parser::with_env(src, &mut resolver, &mut type_reg, module_src);
    let ast_param_def = test(AstParamDef::parse(&vec![
        "std".to_string(),
        "MyData".to_string(),
    ].into(), &mut parser));
    info!("AST param-def = {ast_param_def:?}");

    let env_id = resolver.find_top_level_env(
        &vec!["std".to_string(), "MyData".to_string()].into(), &type_reg).unwrap();
    let mut phase = HirPhase::new(type_reg, resolver);
    let mut hir_param_def = test(ast_param_def.hir_repr(&mut phase));
    info!("HIR param-def = {hir_param_def:?}");

    let edl_param_def = test(hir_param_def.edl_repr(env_id, &mut phase));
    info!("EDL param-def = {edl_param_def:?}");
}

#[test]
fn test_parse_param_def_elicit() {
    let _ = setup_logger();
    let src = r#"<_, _>"#;
    let module_src = inline_code!(src);

    let mut resolver = TopLevelNameResolver::default();
    let mut type_reg = EdlTypeRegistry::default();

    // register types
    populate_test_resolver(&mut resolver, &mut type_reg);
    resolver.push_module("test".to_string());

    let mut parser = Parser::with_env(src, &mut resolver, &mut type_reg, module_src);
    let ast_param_def = test(AstParamDef::parse(&vec![
        "std".to_string(),
        "MyData".to_string(),
    ].into(), &mut parser));
    info!("AST param-def = {ast_param_def:?}");

    let env_id = resolver.find_top_level_env(
        &vec!["std".to_string(), "MyData".to_string()].into(), &type_reg).unwrap();
    let mut phase = HirPhase::new(type_reg, resolver);
    let mut hir_param_def = test(ast_param_def.hir_repr(&mut phase));
    info!("HIR param-def = {hir_param_def:?}");

    let edl_param_def = test(hir_param_def.edl_repr(env_id, &mut phase));
    info!("EDL param-def = {edl_param_def:?}");
}

#[test]
fn test_parse_param_env() {
    let _ = setup_logger();
    let src = r#"<T, const N: usize>"#;
    let module_src = inline_code!(src);

    let mut resolver = TopLevelNameResolver::default();
    let mut type_reg = EdlTypeRegistry::default();

    // register types
    populate_test_resolver(&mut resolver, &mut type_reg);
    resolver.push_module("test".to_string());
    resolver.push_use(vec!["core".to_string(), "usize".to_string()].into()).unwrap();

    let mut parser = Parser::with_env(src, &mut resolver, &mut type_reg, module_src);
    let ast_env = test(AstParamEnv::parse(&mut parser));
    info!("AST env = {ast_env:?}");

    let mut phase = HirPhase::new(type_reg, resolver);
    let mut hir_env = test(ast_env.hir_repr(&mut phase));
    info!("HIR env = {hir_env:?}");

    let edl_env = test(hir_env.edl_repr(&mut phase));
    info!("EDL env = {edl_env:?}");
}