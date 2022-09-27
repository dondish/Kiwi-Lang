use super::kstring::KString;

#[repr(C)]
pub union KObjectValue {
    pub int: i64,
    pub float: f64,
    pub string: *const KString
}

#[repr(u32)]
pub enum KObjectType {
    Int,
    Float,
    String
}

#[repr(C)]
#[derive(Debug, PartialEq, Eq)]
pub struct KObject {
    pub value: KObjectValue,
    pub object_type: KObjectType
    // Implemenet GC Here
}
