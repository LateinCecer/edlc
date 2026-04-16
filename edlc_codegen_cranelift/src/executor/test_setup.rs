use std::hash::DefaultHasher;
#[cfg(feature="serde")]
use serde::{Deserialize, Serialize};
use edlc_core::prelude::edl_type::{EdlTypeId, EdlTypeRegistry};
use edlc_core::prelude::edl_var::EdlVarRegistry;
use edlc_core::prelude::{TypeArgument, TypeArguments};
use crate::unwind::PanicError;

#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub enum FnReport {
    /// Test passed without any errors.
    Ok,
    /// Test failed.
    Err(PanicError),
    /// Test failed during setup routine.
    /// The test itself was not executed.
    SetupErr(PanicError),
    /// Test passed without error, but the teardown routine failed.
    TeardownError(PanicError),
}

#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
#[derive(Default, Debug)]
pub struct TestReport {
    reports: Vec<(EdlTypeId, FnReport)>,
}

impl TestReport {
    pub fn insert(&mut self, func: EdlTypeId, report: FnReport) {
        self.reports.push((func, report));
    }

    pub fn iter(&self) -> std::slice::Iter<(EdlTypeId, FnReport)> {
        self.reports.iter()
    }

    pub fn print(&self, types: &EdlTypeRegistry, vars: &EdlVarRegistry) {
        let mut passed: usize = 0;
        let mut failed: usize = 0;
        println!("----------- test results --------------");
        for (func, report) in self.reports.iter() {
            let args = [
                TypeArgument::new_display(&" -- "),
                TypeArgument::new_edl(func),
                TypeArgument::new_display(&": "),
            ];
            let args: TypeArguments<DefaultHasher> = TypeArguments::new(&args);
            match report {
                FnReport::Ok => {
                    passed += 1;
                    println!("{}OK", args.printable(types, vars));
                }
                FnReport::Err(err) => {
                    failed += 1;
                    println!("{}{err}", args.printable(types, vars));
                }
                FnReport::TeardownError(err) => {
                    failed += 1;
                    println!("{}<TEARDOWN> {err}", args.printable(types, vars));
                }
                FnReport::SetupErr(err) => {
                    failed += 1;
                    println!("{}<SETUP> {err}", args.printable(types, vars));
                }
            }
        }
        println!("------------- summary -----------------");
        println!(" {} tests run", passed + failed);
        println!(" {passed} tests passed");
        println!(" {failed} tests failed");
        println!("---------------------------------------");
    }

    /// Returns true when *all* tests passed.
    pub fn success(&self) -> bool {
        self.reports.iter().all(|(_, item)| matches!(item, FnReport::Ok))
    }
}
