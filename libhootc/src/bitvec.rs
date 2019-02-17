use std::iter;
use std::marker::PhantomData;

fn into_byte_bit(i: usize) -> (usize, u8) {
    (i >> 3, (i & 0x7) as u8)
}

fn from_byte_bit(byte: usize, bit: u8) -> usize {
    (byte << 3) + (bit as usize)
}

#[derive(Debug, PartialEq, Eq)]
pub struct BitVec<T: Into<usize>> {
    bits: Vec<u8>,
    _data: PhantomData<fn (T) -> ()>
}

impl <T: Into<usize>> BitVec<T> {
    pub fn new() -> BitVec<T> {
        BitVec { bits: vec![], _data: PhantomData }
    }

    pub fn clear(&mut self) {
        for b in self.bits.iter_mut() {
            *b = 0;
        };
    }

    pub fn get(&self, i: T) -> bool {
        let (byte, bit) = into_byte_bit(i.into());

        if byte < self.bits.len() {
            ((self.bits[byte] >> bit) & 0x1) != 0
        } else {
            false
        }
    }

    pub fn set(&mut self, i: T, val: bool) -> bool {
        let (byte, bit) = into_byte_bit(i.into());

        if byte < self.bits.len() {
            let old = self.bits[byte];
            let new = if val {
                old | (1 << bit)
            } else {
                old & !(1 << bit)
            };

            self.bits[byte] = new;
            ((old >> bit) & 0x1) != 0
        } else {
            self.bits.reserve(byte + 1);
            self.bits.extend(iter::repeat(0).take(byte + 1 - self.bits.len()));

            let new = if val { 1 << bit } else { 0 };

            self.bits[byte] = new;
            false
        }
    }

    pub fn intersect(&mut self, other: &BitVec<T>) -> bool {
        let mut modified = false;

        for (b1, b2) in self.bits.iter_mut().zip(other.bits.iter()) {
            let b = *b1 & *b2;
            modified = modified || b != *b1;
            *b1 = b;
        };
        for b in self.bits.iter_mut().skip(other.bits.len()) {
            modified = true;
            *b = 0;
        };

        modified
    }

    pub fn union(&mut self, other: &BitVec<T>) -> bool {
        let mut modified = false;

        for (b1, b2) in self.bits.iter_mut().zip(other.bits.iter()) {
            let b = *b1 | *b2;
            modified = modified || b != *b1;
            *b1 = b;
        };
        if other.bits.len() > self.bits.len() {
            modified = true;
            self.bits.extend_from_slice(&other.bits[self.bits.len()..]);
        };

        modified
    }
}

impl <T: Into<usize>> Default for BitVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl <T: Into<usize>> Clone for BitVec<T> {
    fn clone(&self) -> BitVec<T> {
        BitVec { bits: self.bits.clone(), _data: PhantomData }
    }

    fn clone_from(&mut self, source: &BitVec<T>) {
        self.bits.clone_from(&source.bits);
    }
}

impl <T: Into<usize> + From<usize>> BitVec<T> {
    pub fn iter(&self) -> BitVecIter<T> {
        BitVecIter(self, 0)
    }
}

pub struct BitVecIter<'a, T: Into<usize> + From<usize>>(&'a BitVec<T>, usize);

impl <'a, T: Into<usize> + From<usize>> Iterator for BitVecIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        let (mut byte, bit) = into_byte_bit(self.1);

        if byte >= self.0.bits.len() {
            return None;
        };

        let tz = (self.0.bits[byte] >> bit).trailing_zeros();

        if tz != 8 {
            let result = from_byte_bit(byte, bit + tz as u8);

            self.1 = result + 1;
            return Some(T::from(result));
        };

        for &val in self.0.bits[(byte + 1)..].iter() {
            byte += 1;
            let tz = val.trailing_zeros();

            if tz != 8 {
                let result = from_byte_bit(byte, tz as u8);

                self.1 = result + 1;
                return Some(T::from(result));
            };
        };

        self.1 = !0;
        None
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use super::BitVec;

    #[test]
    fn test_get_set() {
        let mut bv: BitVec<usize> = BitVec::new();

        assert!(!bv.get(0));
        assert!(!bv.get(7));
        assert!(!bv.get(8));

        bv.set(7, true);

        assert!(!bv.get(0));
        assert!(bv.get(7));
        assert!(!bv.get(8));

        bv.set(8, false);

        assert!(!bv.get(0));
        assert!(bv.get(7));
        assert!(!bv.get(8));

        bv.set(8, true);

        assert!(!bv.get(0));
        assert!(bv.get(7));
        assert!(bv.get(8));

        bv.set(7, false);

        assert!(!bv.get(0));
        assert!(!bv.get(7));
        assert!(bv.get(8));

        bv.set(0, true);
        bv.set(8, false);

        assert!(bv.get(0));
        assert!(!bv.get(7));
        assert!(!bv.get(8));
    }

    #[test]
    fn test_clear() {
        let mut bv: BitVec<usize> = BitVec::new();

        bv.set(0, true);
        bv.set(8, true);
        bv.set(9, true);
        bv.clear();

        assert!(!bv.get(0));
        assert!(!bv.get(1));
        assert!(!bv.get(8));
        assert!(!bv.get(9));
    }

    #[test]
    fn test_intersect() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(0, true);
        bv1.set(1, true);
        bv2.set(1, true);
        bv2.set(3, true);

        bv1.intersect(&bv2);

        assert!(!bv1.get(0));
        assert!(bv1.get(1));
        assert!(!bv1.get(2));
        assert!(!bv1.get(3));
    }

    #[test]
    fn test_intersect_bv1_longer() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(8, true);
        bv2.set(0, true);

        bv1.intersect(&bv2);

        assert!(!bv1.get(0));
        assert!(!bv1.get(8));
    }

    #[test]
    fn test_intersect_bv2_longer() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(0, true);
        bv2.set(8, true);

        bv1.intersect(&bv2);

        assert!(!bv1.get(0));
        assert!(!bv1.get(8));
    }

    #[test]
    fn test_union() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(0, true);
        bv1.set(1, true);
        bv2.set(1, true);
        bv2.set(3, true);

        bv1.union(&bv2);

        assert!(bv1.get(0));
        assert!(bv1.get(1));
        assert!(!bv1.get(2));
        assert!(bv1.get(3));
    }

    #[test]
    fn test_union_bv1_longer() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(8, true);
        bv2.set(0, true);

        bv1.union(&bv2);

        assert!(bv1.get(0));
        assert!(bv1.get(8));
    }

    #[test]
    fn test_union_bv2_longer() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(0, true);
        bv2.set(8, true);

        bv1.union(&bv2);

        assert!(bv1.get(0));
        assert!(bv1.get(8));
    }

    #[test]
    fn test_iter() {
        let mut bv: BitVec<usize> = BitVec::new();

        bv.set(0, true);
        bv.set(7, true);
        bv.set(8, true);
        bv.set(9, true);
        bv.set(16, true);

        assert_eq!(
            bv.iter().collect_vec(),
            vec![0, 7, 8, 9, 16]
        );
    }
}
