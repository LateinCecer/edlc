/*
 * EDLc, a compiler for the EDL programming language.
 * Copyright (C) 2026  Adrian Paskert
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
use crate::executor::{CatchUnwind, CraneliftJIT, FnReport, JITBencher, TestReport};
use edlc_core::prelude::edl_type::EdlTypeId;

pub(crate) struct UnitTest {
    pub(crate) id: EdlTypeId,
    pub(crate) stub: extern "C" fn(),
    pub(crate) setup: Vec<extern "C" fn()>,
    pub(crate) teardown: Vec<extern "C" fn()>,
}

/// A test executor can be used to run tests.
/// It is essentially a wrapper around pre-identified unit tests in a module.
/// After the creation of the [TestExec], all test entry points will have been created and
/// compiled to executable `extern "C" fn()` function stubs.
pub struct TestExec {
    pub(crate) stubs: Vec<UnitTest>,
}

impl TestExec {
    pub fn run<R: 'static>(&self, compiler: &CraneliftJIT<R>) -> Result<TestReport, anyhow::Error> {
        let mut report = TestReport::default();
        'outer: for UnitTest { id, stub, setup, teardown } in self.stubs.iter() {
            for setup_fn in setup.into_iter() {
                if let Err(err) = compiler.catch_unwind(*setup_fn, ()) {
                    report.insert(*id, FnReport::SetupErr(err));
                    continue 'outer;
                }
            }
            match compiler.catch_unwind(*stub, ()) {
                Ok(()) => (),
                Err(err) => {
                    report.insert(*id, FnReport::Err(err));
                    continue 'outer;
                },
            }
            for teardown_fn in teardown.into_iter() {
                if let Err(err) = compiler.catch_unwind(*teardown_fn, ()) {
                    report.insert(*id, FnReport::TeardownError(err));
                    continue 'outer;
                }
            }
            report.insert(*id, FnReport::Ok);
        }
        Ok(report)
    }

    pub fn bench<R: 'static>(
        &self,
        bencher: &mut impl JITBencher,
        compiler: &CraneliftJIT<R>,
    ) -> Result<TestReport, anyhow::Error> {
        let mut report = TestReport::default();
        for UnitTest { id, stub, setup, teardown } in self.stubs.iter() {
            let full_name = compiler.compiler.phase.types
                .get_fn_qualifier(*id)?
                .as_ref()
                .unwrap();
            let n = bencher.start_bench(full_name);
            let mut ok = true;
            'outer: for _ in 0..n {
                for setup_fn in setup.iter() {
                    if let Err(err) = compiler.catch_unwind(*setup_fn, ()) {
                        report.insert(*id, FnReport::SetupErr(err));
                        ok = false;
                        break 'outer;
                    }
                }
                bencher.start_round();
                let res = compiler.catch_unwind(*stub, ());
                bencher.stop_round();
                if let Err(err) = res {
                    report.insert(*id, FnReport::Err(err));
                    ok = false;
                    break 'outer;
                }

                for teardown_fn in teardown.iter() {
                    if let Err(err) = compiler.catch_unwind(*teardown_fn, ()) {
                        report.insert(*id, FnReport::TeardownError(err));
                        ok = false;
                        break 'outer;
                    }
                }
            }
            if ok {
                report.insert(*id, FnReport::Ok);
            }
            bencher.end_bench();
        }
        Ok(report)
    }
}
