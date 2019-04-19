use std::collections::VecDeque;
use std::mem;

use crate::il::IlRegister;
use crate::util::empty_mut_slice;

#[derive(Debug, Clone)]
pub struct RegisterAllocatorBase<'a, T: Copy + Eq> {
    all_regs: &'a [T],
    available_regs: VecDeque<T>,
    allocated_regs: VecDeque<(IlRegister, T, bool)>
}

impl <'a, T: Copy + Eq> RegisterAllocatorBase<'a, T> {
    pub fn new(all_regs: &'a [T]) -> RegisterAllocatorBase<T> {
        RegisterAllocatorBase {
            all_regs,
            available_regs: all_regs.iter().cloned().collect(),
            allocated_regs: VecDeque::with_capacity(all_regs.len())
        }
    }

    pub fn reset<F: FnMut (IlRegister, T) -> ()>(&mut self, mut spill_handler: F) {
        self.available_regs.clear();
        self.available_regs.extend(self.all_regs.iter().cloned());

        for (virt, real, dirty) in self.allocated_regs.drain(..) {
            if dirty {
                spill_handler(virt, real);
            };
        };
    }

    pub fn try_get(&self, virt: IlRegister) -> Option<T> {
        self.allocated_regs.iter().find(|&&(v, _, _)| v == virt).map(|&(_, r, _)| r)
    }

    pub fn try_get_in_real(&self, real: T) -> Option<(IlRegister, bool)> {
        self.allocated_regs.iter().find(|&&(_, r, _)| r == real).map(|&(v, _, d)| (v, d))
    }

    pub fn try_allocate(&mut self, virt: IlRegister, preferred: Option<T>) -> Option<T> {
        debug_assert!(self.try_get(virt).is_none());

        if let Some(preferred) = preferred {
            if let Some((i, _)) = self.available_regs.iter().enumerate().find(|(_, &r)| r == preferred) {
                self.available_regs.remove(i);
                self.allocated_regs.push_back((virt, preferred, false));
                return Some(preferred);
            };
        };

        if let Some(reg) = self.available_regs.pop_front() {
            self.allocated_regs.push_back((virt, reg, false));
            Some(reg)
        } else {
            None
        }
    }

    pub fn try_allocate_in(&mut self, virt: IlRegister, real: T) -> bool {
        if let Some((i, _)) = self.available_regs.iter().enumerate().find(|&(_, &r)| r == real) {
            self.available_regs.remove(i);
            self.allocated_regs.push_back((virt, real, false));

            true
        } else {
            false
        }
    }

    pub fn try_allocate_with_spill<F: FnMut (&(IlRegister, T, bool)) -> i32>(
        &mut self, virt: IlRegister, mut spill_heuristic: F
    ) -> Option<(T, Option<IlRegister>)> {
        debug_assert!(self.available_regs.is_empty());

        let spill = self.allocated_regs.iter().enumerate().max_by_key(|&(_, data)| spill_heuristic(data));

        if let Some((spill, _)) = spill {
            let (old_virt, real, dirty) = self.allocated_regs.remove(spill).unwrap();

            self.allocated_regs.push_back((virt, real, false));
            if dirty {
                Some((real, Some(old_virt)))
            } else {
                Some((real, None))
            }
        } else {
            None
        }
    }

    pub fn mark_dirty(&mut self, virt: IlRegister) {
        let &mut (_, _, ref mut dirty) = self.allocated_regs.iter_mut().find(|&&mut (v, _, _)| v == virt).unwrap();
        *dirty = true;
    }

    pub fn free(&mut self, virt: IlRegister) -> (T, bool) {
        let (i, _) = self.allocated_regs.iter().enumerate().find(|&(_, &(v, _, _))| v == virt).unwrap();
        let (_, reg, dirty) = self.allocated_regs.remove(i).unwrap();

        self.available_regs.push_back(reg);
        (reg, dirty)
    }

    pub fn try_move_to(&mut self, virt: IlRegister, real: T) -> bool {
        let entry = self.allocated_regs.iter_mut().find(|&&mut (v, _, _)| v == virt);

        if let Some(&mut (_, ref mut old_real, _)) = entry {
            if let Some((i, _)) = self.available_regs.iter().enumerate().find(|&(_, &r)| r == real) {
                self.available_regs.remove(i);
                self.available_regs.push_back(*old_real);
                *old_real = real;

                true
            } else {
                false
            }
        } else {
            self.try_allocate_in(virt, real)
        }
    }

    pub fn try_swap(&mut self, r1: IlRegister, r2: IlRegister) -> bool {
        let i1 = self.allocated_regs.iter().enumerate().find(|&(_, &(v, _, _))| v == r1).map(|(i, _)| i);
        let i2 = self.allocated_regs.iter().enumerate().find(|&(_, &(v, _, _))| v == r2).map(|(i, _)| i);

        if let (Some(i1), Some(i2)) = (i1, i2) {
            let real_r1 = self.allocated_regs[i1].2;
            let real_r2 = self.allocated_regs[i2].2;

            self.allocated_regs[i1].2 = real_r2;
            self.allocated_regs[i2].2 = real_r1;

            true
        } else {
            false
        }
    }

    pub fn spill_all<'b>(&'b mut self) -> SpillAll<'b, 'a, T> {
        SpillAll(self)
    }

    pub fn undirty_all(&mut self) -> UndirtyAll<T> {
        let (a, b) = self.allocated_regs.as_mut_slices();
        UndirtyAll(a, b)
    }
}

pub struct SpillAll<'a, 'b, T: Copy + Eq>(&'a mut RegisterAllocatorBase<'b, T>);

impl <'a, 'b, T: Copy + Eq> Iterator for SpillAll<'a, 'b, T> {
    type Item = (T, IlRegister);

    fn next(&mut self) -> Option<(T, IlRegister)> {
        while let Some((virt, real, dirty)) = self.0.allocated_regs.pop_back() {
            self.0.available_regs.push_back(real);
            if dirty {
                return Some((real, virt));
            };
        };

        None
    }
}

impl <'a, 'b, T: Copy + Eq> Drop for SpillAll<'a, 'b, T> {
    fn drop(&mut self) {
        for _ in self {};
    }
}

pub struct UndirtyAll<'a, T: Copy + Eq>(&'a mut [(IlRegister, T, bool)], &'a mut [(IlRegister, T, bool)]);

impl <'a, T: Copy + Eq> Iterator for UndirtyAll<'a, T> {
    type Item = (T, IlRegister);

    fn next(&mut self) -> Option<(T, IlRegister)> {
        while !self.0.is_empty() {
            let (next, rem) = mem::replace(&mut self.0, empty_mut_slice()).split_at_mut(1);
            self.0 = rem;

            let next = &mut next[0];

            if next.2 {
                next.2 = false;
                return Some((next.1, next.0));
            };
        };

        while !self.1.is_empty() {
            let (next, rem) = mem::replace(&mut self.1, empty_mut_slice()).split_at_mut(1);
            self.1 = rem;

            let next = &mut next[0];

            if next.2 {
                next.2 = false;
                return Some((next.1, next.0));
            };
        };

        None
    }
}

impl <'a, T: Copy + Eq> Drop for UndirtyAll<'a, T> {
    fn drop(&mut self) {
        for _ in self {};
    }
}
