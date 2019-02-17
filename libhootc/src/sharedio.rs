use std::cell::RefCell;
use std::io;
use std::rc::Rc;

pub struct SharedWrite<'a, T: io::Write>(Rc<RefCell<&'a mut T>>) where T: ?Sized, T: 'a;

impl <'a, T: io::Write> SharedWrite<'a, T> where T: ?Sized, T: 'a {
    pub fn new(w: &mut T) -> SharedWrite<T> {
        SharedWrite(Rc::new(RefCell::new(w)))
    }
}

impl <'a, T: io::Write> io::Write for SharedWrite<'a, T> where T: ?Sized, T: 'a {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.borrow_mut().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.borrow_mut().flush()
    }
}

impl <'a, T: io::Write> Clone for SharedWrite<'a, T> where T: ?Sized, T: 'a {
    fn clone(&self) -> SharedWrite<'a, T> {
        SharedWrite(self.0.clone())
    }
}
