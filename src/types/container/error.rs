use super::associative as assoc;
use crate::derivation::*;
use crate::traits::*;
use crate::types::container::{self as coll, Mutey};
use crate::types::number::Int;
use crate::types::{Item, Word};
use crate::{fit, list};
use std::convert::Infallible;

/// Represents a runtime error type. Contains generic fields to hold
/// things like what type of error, the actual vs expected conditions,
/// etc. Also holds whether the error has been handled or not, which
/// the runtime uses to decide whether to keep unwinding the program
/// looking for something to handle the error. An error that has been
/// handled is inert, it is just another data value.
#[derive(Clone, PartialEq)]
pub struct Error {
    pub data: assoc::Association,
    pub is_handled: bool,
    pub cause: Option<Box<Error>>,
}

impl Identity for Error {}

impl Error {
    /// Nests another error as the cause of this error.
    pub fn nest(mut self, cause: Error) -> Self {
        self.cause = Some(Box::new(cause));
        self
    }

    /// Creates a new error.
    pub fn create<T: Fit<Item>>(asked: coll::List, reason: &str, actual: Option<T>) -> Error {
        // let bt = backtrace::Backtrace::new();
        let mut data: Vec<(assoc::KeyItem, Item)> = vec![
            (fit!("type"), fit!("error")),
            (fit!("asked"), asked.fit()),
            (fit!("reason"), reason.to_string().fit()),
            //("backtrace".fit(), Item::String(format!("{:?}", bt))),
        ];
        if let Some(actual) = actual {
            data.push((fit!("actual"), actual.fit()));
        }
        Error {
            is_handled: false,
            data: std::sync::Arc::new(data.rewrap::<assoc::AssociationContent, assoc::Entry>()),
            cause: None,
        }
    }

    /// Creates a stack underflow error for when the current word
    /// needs more items than there are on the stack.
    pub fn stack_underflow() -> Error {
        Error::create(
            list!("consume"),
            "not enough items on stack",
            Option::<Item>::None,
        )
    }

    pub fn overflow() -> Error {
        Error::create(list!("arithmetic"), "number overflow", Option::<Item>::None)
    }

    pub fn undefined(w: Word) -> Error {
        Error::create(list!(w), "word is not defined", Option::<Item>::None)
    }

    pub fn type_mismatch<T: Fit<Item>>(asked: coll::List, actual: Option<T>) -> Error {
        Error::create(asked, "type mismatch", actual)
    }

    pub fn division_by_zero() -> Error {
        Error::create(list!("/"), "division by zero", Option::<Item>::None)
    }

    pub fn expected<T: Fit<Item>>(typestr: Word, actual: T) -> Error {
        Error::type_mismatch(list!(typestr), Some(actual))
    }

    pub fn short_list(expected: Int) -> Error {
        Error::create(
            list!("count", expected, ">="),
            "list had too few items",
            Option::<Item>::None,
        )
    }

    pub fn list_count(expected: Int) -> Error {
        Error::create(
            list!("count", expected, "="),
            "list had wrong number of items",
            Option::<Item>::None,
        )
    }

    pub fn negative(actual: Int) -> Error {
        Error::too_small(actual, 0)
    }

    pub fn too_small(actual: Int, expected: Int) -> Error {
        Error::create(list!(expected, ">="), "number too small", Some(actual))
    }

    pub fn too_large(actual: Int, expected: Int) -> Error {
        Error::create(list!(expected, "<="), "number too large", Some(actual))
    }

    pub fn out_of_range(actual: Int, min: Int, max: Int) -> Error {
        Error::create(
            list!(min, max, "range?"),
            "number out of range",
            Some(actual),
        )
    }

    pub fn parse(reason: &str) -> Error {
        Error::create(list!("read"), reason, Option::<Item>::None)
    }

    pub fn test_assertion(
        program: coll::List,
        expected_prog: coll::List,
        actual_stack: coll::List,
    ) -> Error {
        let mut e = Error::create(program, "assertion failed", Some(actual_stack));
        let d = e.data.mutate();
        d.insert(fit!("expected-program"), expected_prog.fit());
        e
    }

    pub fn actual(&self) -> Option<&Item> {
        self.data
            .get(&(assoc::KeyItem::Word(Word::try_derive("actual").unwrap())))
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn push(&mut self, key: assoc::KeyItem, value: Item) -> Option<Item> {
        self.data.mutate().insert(key, value)
    }

    pub fn add_trace(&mut self, trace: coll::List) {
        self.data.mutate().insert(fit!("trace"), trace.fit());
    }
}

impl Derive<Infallible> for Error {
    fn derive(_x: Infallible) -> Self {
        match _x {} // Since Infallible can never be instantiated, this will never run
    }
}

impl<Thunk> Derive<Thunk> for Error
where
    Thunk: FnOnce() -> Error,
{
    fn derive(t: Thunk) -> Self {
        t()
    }
}

impl Derive<Infallible> for Item {
    fn derive(_x: Infallible) -> Self {
        match _x {} // Since Infallible can never be instantiated, this will never run
    }
}

impl Derive<Error> for assoc::Association {
    fn derive(e: Error) -> assoc::Association {
        e.data
    }
}

impl TryDerive<Item> for Error {
    fn try_derive(i: Item) -> Result<Self, Error> {
        match i {
            Item::Error(e) => Ok(e),
            i => Error::try_derive(assoc::Associative::try_derive(i)?),
            //i => Err(Error::expected(fit!("error"), i)),
        }
    }
}

impl TryDerive<Box<dyn Iterator<Item = Item>>> for Error {
    fn try_derive(i: Box<dyn Iterator<Item = Item>>) -> Result<Self, Error> {
        //TODO: this can't fail, can just be a From.
        // Really though, Error should have predefined fields like Environment.
        let list: crate::types::container::List = i.collect();
        let data = std::sync::Arc::new(list.try_rewrap::<assoc::AssociationContent>()?);
        Ok(Error {
            data,
            is_handled: false,
            cause: None,
        })
    }
}

impl TryDerive<assoc::Associative> for Error {
    fn try_derive(a: assoc::Associative) -> Result<Self, Error> {
        match a {
            assoc::Associative::Error(e) => Ok(e),
            assoc::Associative::Assoc(a) => {
                if a.get(&fit!("type")) != Some(&fit!("error")) {
                    Err(Error::expected(fit!("error"), a))
                } else {
                    Ok(Error {
                        data: a.clone(),
                        is_handled: true,
                        cause: None,
                    })
                }
            }
            i => Err(Error::expected(fit!("error"), i)),
        }
    }
}

impl Derive<Error> for Item {
    fn derive(e: Error) -> Item {
        assoc::Associative::Error(e).fit()
    }
}

impl IntoIterator for Error {
    type Item = assoc::Entry;
    type IntoIter = Box<dyn Iterator<Item = assoc::Entry>>;

    fn into_iter(self) -> Self::IntoIter {
        let items: Vec<_> = self
            .data
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .chain(std::iter::once((fit!("handled"), self.is_handled.fit())))
            .collect();
        Box::new(items.into_iter())
    }
}

/// When joining two identically shaped map-like structs, it's just
/// last one wins. This isn't ideal because errors aren't necessarily
/// identical fields (they support arbitrary fields but that's a bit
/// of a implmentation detail for now)
impl coll::Join<Error> for Error {
    type Output = Error;
    type Error = Infallible;
    fn join(self, other: Self) -> Result<Self::Output, Self::Error> {
        Ok(other)
    }
}

pub trait OkItem {}
