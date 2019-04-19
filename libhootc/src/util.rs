use std::ptr::NonNull;
use std::slice;

pub(crate) fn empty_mut_slice<T>() -> &'static mut [T] {
    // It's perfectly safe for this to point to completely invalid memory, since the length of the
    // slice being 0 prevents any data from ever being accessed through this pointer. However, the
    // pointer still has to be non-null and aligned properly to conform with the requirements of
    // from_raw_parts_mut.
    unsafe { slice::from_raw_parts_mut(NonNull::dangling().as_ptr(), 0) }
}
