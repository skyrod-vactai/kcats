use crate::types::container::error::Error;
use crate::types::*;

/// A marker trait for types that can be blanket converted to themselves
pub trait Identity {}

// Define custom traits that mimic std ones
/// a trait similar to [std::convert::From]
pub trait Derive<T>: Sized {
    fn derive(value: T) -> Self;
}

/// Detailed fallible conversion: returns a descriptive Error, potentially with nested causes.
pub trait TryDerive<Src>: Sized {
    fn try_derive(value: Src) -> Result<Self, Error>;
}

/// Fast fallible conversion: returns the original value if it fails.
pub trait TryCast<Src>: Sized {
    fn try_cast(value: Src) -> Result<Self, Src>;
}

/// a trait similar to [std::convert::Into]
pub trait Fit<T>: Sized {
    fn fit(self) -> T;
}

/// a trait similar to [std::convert::TryInto] for TryDerive
pub trait TryFit<Target>: Sized {
    fn try_fit(self) -> Result<Target, Error>;
}

/// a trait similar to [std::convert::TryInto] for TryCast
pub trait TryCastInto<Target>: Sized {
    fn try_cast_into(self) -> Result<Target, Self>;
}

// blanket impls
impl<Src, Target> Fit<Target> for Src
where
    Target: Derive<Src>,
{
    fn fit(self) -> Target {
        Target::derive(self)
    }
}

impl<Src, Target> TryFit<Target> for Src
where
    Target: TryDerive<Src>,
{
    fn try_fit(self) -> Result<Target, Error> {
        Target::try_derive(self)
    }
}

impl<Src, Target> TryCastInto<Target> for Src
where
    Target: TryCast<Src>,
{
    fn try_cast_into(self) -> Result<Target, Src> {
        Target::try_cast(self)
    }
}

impl<T> Derive<T> for T
where
    T: Identity,
{
    fn derive(value: T) -> T {
        value
    }
}

impl<T> TryDerive<T> for T
where
    T: Identity + Fit<Item>,
{
    fn try_derive(value: T) -> Result<T, Error> {
        Ok(value)
    }
}

impl<T> TryCast<T> for T
where
    T: Identity,
{
    fn try_cast(value: T) -> Result<T, T> {
        Ok(value)
    }
}

pub trait DeriveIter<I: IntoIterator> {
    fn derive_iter(iter: I) -> Self;
}

impl<I: IntoIterator> DeriveIter<I> for crate::types::container::List
where
    I::Item: Fit<crate::types::Item>,
{
    fn derive_iter(iter: I) -> Self {
        iter.into_iter().map(|x| x.fit()).collect()
    }
}

impl<I: IntoIterator> DeriveIter<I> for crate::types::container::associative::AssociationContent
where
    I::Item: Fit<(
        crate::types::container::associative::KeyItem,
        crate::types::Item,
    )>,
{
    fn derive_iter(iter: I) -> Self {
        iter.into_iter().map(|x| x.fit()).collect()
    }
}

impl<I: IntoIterator> DeriveIter<I> for crate::types::container::associative::Association
where
    I::Item: Fit<(
        crate::types::container::associative::KeyItem,
        crate::types::Item,
    )>,
{
    fn derive_iter(iter: I) -> Self {
        std::sync::Arc::new(iter.into_iter().map(|x| x.fit()).collect())
    }
}
