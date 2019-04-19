use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label(u32);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "L{}", self.0)
    }
}

impl Into<usize> for Label {
    fn into(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub struct LabelAlloc {
    next: Label
}

impl LabelAlloc {
    pub fn new() -> LabelAlloc {
        LabelAlloc { next: Label(0) }
    }

    pub fn alloc(&mut self) -> Label {
        let l = self.next;
        self.next.0 += 1;
        l
    }
}
