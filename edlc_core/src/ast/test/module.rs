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

use log::info;
use crate::ast::{AstModule, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::ast::test::{populate_test_resolver, print_errors, test};
use crate::core::edl_type::EdlTypeRegistry;
use crate::hir::{HirModule, HirPhase};
use crate::parser::Parser;
use crate::resolver::TopLevelNameResolver;
use crate::{inline_code, setup_logger};
use crate::core::type_analysis::InferState;

fn translate_module(src: &'static str) -> Result<(HirModule, HirPhase), AstTranslationError> {
    let mut res = TopLevelNameResolver::default();
    let mut type_reg = EdlTypeRegistry::default();
    populate_test_resolver(&mut res, &mut type_reg);

    res.push_module("test".to_string());
    let mut parser = Parser::with_env(src, &mut res, &mut type_reg, inline_code!(src));
    let mut module = test(AstModule::parse(&mut parser, vec!["test".to_string()].into(), String::new()));

    let mut hir_phase = HirPhase::new(type_reg, res);
    module.prepare_root(&mut hir_phase)?;
    let hir_module = module.hir_repr(&mut hir_phase)?;
    Ok((hir_module, hir_phase))
}

#[test]
fn test_name_resolution() {
    setup_logger().unwrap();
    let src = r#"
    use core::usize;

    // this reads as `i32`, since that is the default type for integer literals
    const N: usize = 4;
    // this reads as `usize`, since that is the type specified at the LHS
    const DIM: usize = 3;
    // this reads as `usize`, since that is the type specified at the RHS
    const NSIZE: usize = 3_usize;

    let pi = 3.1415_f64;
    let a: usize = 3;
    let b: usize = 4_usize;
    let mut c: core::str = "Hello, world!";
    "#;

    let (mut hir_module, mut hir_phase) = test(translate_module(src));

    info!("module: {hir_module:?}");


    // unlike variables, constant values are pushed to the name resolver **before** the types are translated from AST to HIR
    assert!(hir_phase.res.find_top_level_const(&vec!["N".to_string()].into()).is_some());
    assert!(hir_phase.res.find_top_level_const(&vec!["DIM".to_string()].into()).is_some());
    assert!(hir_phase.res.find_top_level_const(&vec!["NSIZE".to_string()].into()).is_some());


    assert!(hir_phase.res.find_top_level_var(&vec!["pi".to_string()].into()).is_none());
    assert!(hir_phase.res.find_top_level_var(&vec!["a".to_string()].into()).is_none());
    assert!(hir_phase.res.find_top_level_var(&vec!["b".to_string()].into()).is_none());
    assert!(hir_phase.res.find_top_level_var(&vec!["c".to_string()].into()).is_none());
    let mut errors = vec![];
    hir_module.name_resolve(&mut hir_phase, &mut errors);
    print_errors(errors);
    info!("module: {hir_module:?}");

    assert!(hir_phase.res.find_top_level_const(&vec!["N".to_string()].into()).is_some());
    assert!(hir_phase.res.find_top_level_const(&vec!["DIM".to_string()].into()).is_some());
    assert!(hir_phase.res.find_top_level_const(&vec!["NSIZE".to_string()].into()).is_some());

    assert!(hir_phase.res.find_top_level_var(&vec!["pi".to_string()].into()).is_some());
    assert!(hir_phase.res.find_top_level_var(&vec!["a".to_string()].into()).is_some());
    assert!(hir_phase.res.find_top_level_var(&vec!["b".to_string()].into()).is_some());
    assert!(hir_phase.res.find_top_level_var(&vec!["c".to_string()].into()).is_some());
}

#[test]
fn test_unresolvable_const_type() {
    setup_logger().unwrap();
    let (mut module, mut phase) = test(translate_module(r#"
    use core::f64;
    const N: f64 = 0_usize;"#));
    let mut errors = vec![];
    module.name_resolve(&mut phase, &mut errors);
    assert!(!errors.is_empty());

    let (mut module, mut phase) = test(translate_module(r#"
    use core::usize;
    const N: usize = 3_f64;"#));
    let mut errors = vec![];
    module.name_resolve(&mut phase, &mut errors);
    assert!(!errors.is_empty());

    let (mut module, mut phase) = test(translate_module(r#"
    use core::usize;
    const N: usize = 3.14;"#));
    let mut errors = vec![];
    module.name_resolve(&mut phase, &mut errors);
    assert!(!errors.is_empty());
}

#[test]
fn test_module_transform() {
    setup_logger().unwrap();
    let src = r#"
    use core::usize;
    use core::f64;
    use core::f32;
    use core::i32;

    let index = 0;
    let config = "config.exp";
    const DIM: usize = std::load_spatial_dims(config, std::add_42(index));
    let domain: std::Domain<f64, 4, _> = std::load_domain::<_, _, DIM>();

    let mut dim = std::add_42(std::add_42::<f32>(std::add_42(3.14159265)));
    "#;
    let (mut module, mut phase) = test(translate_module(src));

    let mut infer_state = InferState::new();
    print_errors(module.transform(&mut phase, &mut infer_state));
    print_errors(module.verify(&mut phase, &mut infer_state));

    info!("{:#?}", module);
}
