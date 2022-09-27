#[repr(C)]
pub struct KString {
    pub ptr: *const str,
    pub size: u64
}
