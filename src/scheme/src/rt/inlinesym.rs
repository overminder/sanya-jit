use std::mem::transmute;
use std::str;

#[derive(Clone, Copy)]
pub struct InlineSym {
    length: u8,
    encoding: [u8; 7],
}

impl InlineSym {
    pub fn from_str(s: &str) -> Option<Self> {
        if s.len() < 8 {
            let mut bs = [0; 7];
            for (dst, src) in bs.iter_mut().zip(s.as_bytes()) {
                *dst = *src;
            }

            Some(InlineSym {
                length: s.len() as u8,
                encoding: bs,
            })
        } else {
            None
        }
    }

    pub fn as_str(&self) -> &str {
        str::from_utf8(&self.encoding[..self.length as usize]).unwrap()
    }

    pub fn from_word(w: usize) -> Self {
        unsafe { transmute(w) }
    }

    pub fn as_word(&self) -> usize {
        unsafe { transmute(*self) }
    }
}
