extern crate libc;

use libc::c_void;
use libc::c_double;
use std::marker::PhantomData;
use std::mem;

enum Void { }

#[repr(C)]
pub struct StablePtr<T> (*mut StablePtr<T>);

#[repr(C)]
pub struct UnownedStablePtr<T> (*mut StablePtr<T>);

pub struct Maybe<T> {
    _void:  Void,
    _dummy: PhantomData<T>
}

extern "C" {
    fn hs_init(_ : *mut i32, _ : *mut *mut *mut u8) -> ();
    fn hs_free_stable_ptr(_ : *mut c_void) -> ();
    fn from_maybe(_: UnownedStablePtr<Maybe<()>>) -> StablePtr<()>;
    fn get_double(_: UnownedStablePtr<c_double>) -> c_double;
}

impl<T> StablePtr<T> {
    pub fn get<U>(&self) -> U
            where T: Get<U> {
        Get::get(self.get_unowned())
    }

    pub fn get_unowned(&self) -> UnownedStablePtr<T> {
        UnownedStablePtr(self.0)
    }

    pub fn into_raw(self) -> *mut c_void {
        unsafe { mem::transmute(self) }
    }

    pub unsafe fn from_raw(x : *mut c_void) -> StablePtr<T> {
        StablePtr (x as *mut StablePtr<T>)
    }

    pub unsafe fn cast<U>(self) -> StablePtr<U> {
        mem::transmute(self)
    }

    pub unsafe fn cast_ref<U>(&self) -> &StablePtr<U> {
        mem::transmute(self)
    }
}

impl<T> UnownedStablePtr<T> {
    pub fn get<U>(self) -> U
            where T: Get<U> {
        Get::get(self)
    }

    pub fn into_raw(self) -> *mut c_void {
        self.0 as *mut c_void
    }

    pub unsafe fn from_raw(x : *mut c_void) -> UnownedStablePtr<T> {
        UnownedStablePtr(x as *mut StablePtr<T>)
    }

    pub unsafe fn cast<U>(self) -> UnownedStablePtr<U> {
        UnownedStablePtr(self.0 as *mut StablePtr<U>)
    }

    pub unsafe fn cast_ref<U>(&self) -> &UnownedStablePtr<U> {
        mem::transmute(self)
    }
}

impl<T> Eq for StablePtr<T> { }
impl<T> Eq for UnownedStablePtr<T> { }
impl<T,U> PartialEq<StablePtr<U>> for StablePtr<T> {
    fn eq(&self, other: &StablePtr<U>) -> bool {
        (self.0 as usize) == (other.0 as usize)
    }
}

impl<T,U> PartialEq<StablePtr<U>> for UnownedStablePtr<T> {
    fn eq(&self, other: &StablePtr<U>) -> bool {
        (self.0 as usize) == (other.0 as usize)
    }
}

impl<T,U> PartialEq<UnownedStablePtr<U>> for StablePtr<T> {
    fn eq(&self, other: &UnownedStablePtr<U>) -> bool {
        (self.0 as usize) == (other.0 as usize)
    }
}

impl<T,U> PartialEq<UnownedStablePtr<U>> for UnownedStablePtr<T> {
    fn eq(&self, other: &UnownedStablePtr<U>) -> bool {
        (self.0 as usize) == (other.0 as usize)
    }
}

impl<T> Drop for StablePtr<T> {
    fn drop(&mut self) {
        unsafe {
            #[cfg(debug_assertions)]
            println!("Freeing StablePtr: {}", self.0 as usize);

            hs_free_stable_ptr(self.0 as *mut c_void);
        }
    }
}

impl<T> Copy for UnownedStablePtr<T> { }
impl<T> Clone for UnownedStablePtr<T> {
    fn clone(&self) -> UnownedStablePtr<T> {
        *self
    }
}

pub trait Get<T>
        where Self: Sized {
    fn get(x: UnownedStablePtr<Self>) -> T;
}

impl Get<c_double> for c_double {
    fn get(x: UnownedStablePtr<c_double>) -> c_double {
        unsafe { get_double(x) }
    }
}

impl<T> Get<Option<StablePtr<T>>> for Maybe<T> {
    fn get(x: UnownedStablePtr<Maybe<T>>) -> Option<StablePtr<T>> {
        unsafe {
            let result = from_maybe(x.cast());
            if x.eq(&result) {
                mem::forget(result);
                None
            } else {
                Some(result.cast())
            }
        }
    }
}

pub fn init() {
    unsafe {
        hs_init(std::ptr::null_mut(), std::ptr::null_mut());
    }
}
