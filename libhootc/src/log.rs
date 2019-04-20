use std::io::Write;

use crate::sharedio::SharedWrite;

pub struct Log<'a>(pub Option<&'a mut Write>);

impl <'a> Log<'a> {
    pub fn try_unwrap(&mut self) -> Option<&mut Write> {
        if let Some(ref mut w) = self.0 {
            Some(*w)
        } else {
            None
        }
    }

    #[deprecated]
    pub fn unwrap(&mut self) -> &mut Write {
        self.try_unwrap().unwrap()
    }

    pub fn share(&mut self) -> SharedLog {
        SharedLog(if let Some(ref mut w) = self.0 {
            Some(SharedWrite::new(*w))
        } else {
            None
        })
    }

    pub fn log(&mut self) -> &mut Log<'a> {
        self
    }
}

#[derive(Clone)]
pub struct SharedLog<'a>(pub Option<SharedWrite<'a, dyn Write>>);

impl <'a> SharedLog<'a> {
    pub fn log(&mut self) -> Log {
        Log(if let Some(ref mut w) = self.0 {
            Some(w)
        } else {
            None
        })
    }
}

#[macro_export]
macro_rules! log_write {
    ($dst:expr, $($arg:tt)*) => {
        if let Log(Some(ref mut dst)) = $dst.log() {
            write!(dst, $($arg)*).unwrap();
        };
    }
}

#[macro_export]
macro_rules! log_writeln {
    ($dst:expr) => {
        if let crate::log::Log(Some(ref mut dst)) = $dst.log() {
            writeln!(dst).unwrap();
        };
    };
    ($dst:expr, $($arg:tt)*) => {
        if let crate::log::Log(Some(ref mut dst)) = $dst.log() {
            writeln!(dst, $($arg)*).unwrap();
        };
    }
}
