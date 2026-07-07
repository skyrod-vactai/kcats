//! Support for numbers in kcats. Currently just [i64] and [f64], but
//! this module will eventually support bignums and autopromotion.
use super::container::error::Error;
use crate::derivation::*;
use crate::fit;
use crate::types::Item;
use num_integer::Roots;
use serde::ser::{Serialize, Serializer};
use std::num::{ParseFloatError, ParseIntError};
use std::ops::Div;
use std::ops::Rem;
/// An integer type
pub type Int = i64;

/// A floating point type
pub type Float = f64;

#[derive(Clone, Debug)]
pub enum Number {
    Int(Int),
    Float(Float),
}

impl Number {
    pub fn add(&self, other: Number) -> Number {
        match (self, other) {
            (Number::Int(i), Number::Int(j)) => Number::Int(i + j),
            (Number::Float(i), Number::Float(j)) => Number::Float(i + j),
            (Number::Int(i), Number::Float(j)) => Number::Float(*i as Float + j),
            (Number::Float(i), Number::Int(j)) => Number::Float(i + j as Float),
        }
    }

    pub fn subtract(&self, other: Number) -> Number {
        match (self, other) {
            (Number::Int(i), Number::Int(j)) => Number::Int(i - j),
            (Number::Float(i), Number::Float(j)) => Number::Float(i - j),
            (Number::Int(i), Number::Float(j)) => Number::Float(*i as Float - j),
            (Number::Float(i), Number::Int(j)) => Number::Float(i - j as Float),
        }
    }

    pub fn multiply(&self, other: Number) -> Number {
        match (self, other) {
            (Number::Int(i), Number::Int(j)) => Number::Int(i * j),
            (Number::Float(i), Number::Float(j)) => Number::Float(i * j),
            (Number::Int(i), Number::Float(j)) => Number::Float(*i as Float * j),
            (Number::Float(i), Number::Int(j)) => Number::Float(i * j as Float),
        }
    }

    fn div<T>(a: T, b: T) -> Result<Number, Error>
    where
        T: Div<Output = T> + PartialEq + From<i32> + Copy + Fit<Number>,
    {
        if b == T::from(0) {
            return Err(Error::division_by_zero());
        }
        Ok((a / b).fit())
    }

    fn rem<T>(a: T, b: T) -> Result<Number, Error>
    where
        T: Rem<Output = T> + PartialEq + From<i32> + Copy + Fit<Number>,
    {
        if b == T::from(0) {
            return Err(Error::division_by_zero());
        }
        Ok((a % b).fit())
    }

    pub fn divide(i: Number, j: Number) -> Result<Number, Error> {
        match (i, j) {
            (Number::Int(i), Number::Int(j)) => Number::div(i, j),
            (Number::Int(i), Number::Float(j)) => Number::div(i as Float, j),
            (Number::Float(i), Number::Int(j)) => Number::div(i, j as Float),
            (Number::Float(i), Number::Float(j)) => Number::div(i, j),
        }
    }

    pub fn remainder(i: Number, j: Number) -> Result<Number, Error> {
        match (i, j) {
            (Number::Int(i), Number::Int(j)) => Number::rem(i, j),
            (Number::Int(i), Number::Float(j)) => Number::rem(i as Float, j),
            (Number::Float(i), Number::Int(j)) => Number::rem(i, j as Float),
            (Number::Float(i), Number::Float(j)) => Number::rem(i, j),
        }
    }

    pub fn gt(i: Number, j: Number) -> bool {
        match (i, j) {
            (Number::Int(i), Number::Int(j)) => i > j,
            (Number::Float(i), Number::Float(j)) => i > j,
            (Number::Int(i), Number::Float(j)) => i as Float > j,
            (Number::Float(i), Number::Int(j)) => i > j as Float,
        }
    }

    pub fn lt(i: Number, j: Number) -> bool {
        match (i, j) {
            (Number::Int(i), Number::Int(j)) => i < j,
            (Number::Float(i), Number::Float(j)) => i < j,
            (Number::Int(i), Number::Float(j)) => (i as Float) < j,
            (Number::Float(i), Number::Int(j)) => i < j as Float,
        }
    }

    pub fn gte(i: Number, j: Number) -> bool {
        match (i, j) {
            (Number::Int(i), Number::Int(j)) => i >= j,
            (Number::Float(i), Number::Float(j)) => i >= j,
            (Number::Int(i), Number::Float(j)) => (i as Float) >= j,
            (Number::Float(i), Number::Int(j)) => i >= j as Float,
        }
    }

    pub fn lte(i: Number, j: Number) -> bool {
        match (i, j) {
            (Number::Int(i), Number::Int(j)) => i <= j,
            (Number::Float(i), Number::Float(j)) => i <= j,
            (Number::Int(i), Number::Float(j)) => (i as Float) <= j,
            (Number::Float(i), Number::Int(j)) => i <= j as Float,
        }
    }

    pub fn abs(&self) -> Number {
        match self {
            Number::Int(i) => Number::Int(i.abs()),
            Number::Float(f) => Number::Float(f.abs()),
        }
    }

    pub fn sqrt(&self) -> Number {
        match self {
            Number::Int(i) => Number::Int(i.sqrt()),
            Number::Float(f) => Number::Float(f.sqrt()),
        }
    }
}

impl Identity for Number {}
impl Identity for Int {}
impl Identity for Float {}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) => a == b,
            (Number::Float(a), Number::Float(b)) => a == b,
            (Number::Float(a), Number::Int(b)) => *a == *b as Float,
            (Number::Int(a), Number::Float(b)) => *a as Float == *b,
        }
    }
}

impl TryDerive<Number> for Float {
    fn try_derive(i: Number) -> Result<Self, Error> {
        match i {
            Number::Float(i) => Ok(i),
            i => Err(Error::expected(fit!("float"), i)),
        }
    }
}

impl TryDerive<Number> for Int {
    fn try_derive(i: Number) -> Result<Self, Error> {
        match i {
            Number::Int(i) => Ok(i),
            i => Err(Error::expected(fit!("integer"), i)),
        }
    }
}

impl Derive<Int> for Item {
    fn derive(c: Int) -> Self {
        Item::Int(c)
    }
}

impl Derive<Float> for Item {
    fn derive(c: Float) -> Self {
        Item::Float(c)
    }
}

impl From<ParseIntError> for Error {
    fn from(e: ParseIntError) -> Self {
        Error::parse(e.to_string().as_str())
    }
}
impl From<ParseFloatError> for Error {
    fn from(e: ParseFloatError) -> Self {
        Error::parse(e.to_string().as_str())
    }
}

impl TryDerive<Item> for Number {
    fn try_derive(i: Item) -> Result<Self, Error> {
        let fromstr = |s: String| {
            let r = s
                .as_str()
                .parse::<i64>()
                .map(Number::Int)
                .map_err(|_e| Error::expected(fit!("integer"), s.clone()));

            r.or_else(|_| s.as_str().parse::<Float>().map(Number::Float))
                .map_err(|_e| Error::expected(fit!("float"), s))
        };
        match i {
            Item::Int(i) => Ok(Number::Int(i)),
            Item::Float(f) => Ok(Number::Float(f)),
            Item::Char(c) => Ok(Number::Int(c as Int)),
            Item::String(s) => fromstr(*s),
            i => Err(Error::expected(fit!("number"), i)),
        }
    }
}

impl TryDerive<Item> for Int {
    fn try_derive(i: Item) -> Result<Self, Error> {
        match Number::try_derive(i)? {
            Number::Int(i) => Ok(i),
            n => Err(Error::expected(fit!("integer"), Item::derive(n))),
        }
    }
}

impl TryDerive<Item> for Float {
    fn try_derive(i: Item) -> Result<Self, Error> {
        match Number::try_derive(i)? {
            Number::Float(i) => Ok(i),
            i => Err(Error::expected(fit!("float"), Item::derive(i))),
        }
    }
}

impl Derive<Number> for Item {
    fn derive(c: Number) -> Self {
        match c {
            Number::Int(i) => Item::Int(i),
            Number::Float(f) => Item::Float(f),
        }
    }
}

impl Derive<Int> for Number {
    fn derive(c: Int) -> Self {
        Number::Int(c)
    }
}

impl Derive<Float> for Number {
    fn derive(c: Float) -> Self {
        Number::Float(c)
    }
}

impl Serialize for Number {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Number::Int(i) => serializer.serialize_i64(*i),
            Number::Float(f) => serializer.serialize_f64(*f),
        }
    }
}
