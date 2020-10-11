#![allow(unused)]

use super::naive_copying::GcState as Space;
use rt::Universe;

#[repr(C)]
pub struct GcState {
    minor: Space,
    major: Space,
}

impl GcState {
    pub unsafe fn new(major_size: usize) -> Self {
        GcState {
            minor: Space::new(30000), // 64 KB L1 d-cache
            major: Space::new(major_size),
        }
    }

    pub fn set_universe(&mut self, u: &Universe) {
        self.minor.set_universe(u);
        self.major.set_universe(u);
    }
}
