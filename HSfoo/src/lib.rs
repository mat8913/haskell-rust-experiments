extern crate haskell;
extern crate libc;

use haskell::StablePtr;
use haskell::Maybe;
use libc::c_double;

pub use haskell::init;

extern "C" {
    fn hs_sqrt(_: c_double) -> StablePtr<Maybe<c_double>>;
}

pub fn sqrt(x: f64) -> Option<f64> {
    unsafe {
        hs_sqrt(x).get().map(|x| x.get())
    }
}
