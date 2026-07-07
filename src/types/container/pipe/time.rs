use crate::types::container::{associative as assoc, SimpleTake};
use crate::types::number::Int;
use crate::types::*;

use std::time::{SystemTime, UNIX_EPOCH};

pub struct Time;

impl Default for Time {
    fn default() -> Self {
        Self::new()
    }
}

impl Time {
    pub fn new() -> Self {
        Time
    }
}

impl SimpleTake for Time {
    type Item = Int;
    fn take_simple(&mut self) -> Option<Self::Item> {
        let t = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as Int;
        Some(t)
    }
}
pub fn representation() -> Item {
    assoc::Association::derive_iter([
        (fit!("type"), fit!("pipe")),
        (fit!("endpoint"), fit!("systemtime")),
        (
            fit!("out"),
            assoc::Association::derive_iter([
                (fit!("type"), fit!("integer")),
                (fit!("units"), fit!("milliseconds")),
            ])
            .fit(),
        ),
    ])
    .fit()
}
