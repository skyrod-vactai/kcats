use crate::derivation::*;

/// a trait that marks iterable types that can return arbitrary
/// numbers of items. For example lists, maps, etc. But not things
/// like Result or Option.
pub trait IntoList {}

// impl<T> ToIterator for Vec<T> {
//     type Item = T;
//     type IntoIter = std::vec::IntoIter<T>;

//     fn to_iter(self) -> Self::IntoIter {
//         self.into_iter()
//     }
// }

pub trait Fresh {
    fn fresh() -> Self;
}

pub trait RewrapExt: IntoIterator + Sized {
    /// Infallible: Uses Derive. Returns the container directly.
    fn rewrap<Target, U>(self) -> Target
    where
        Target: FromIterator<U>,
        U: Derive<Self::Item>,
    {
        self.into_iter().map(U::derive).collect()
    }

    /// Fallible: Uses Derive specifically for DeriveErrors. Returns Result<Target, E>.

    fn try_rewrap<Target>(self) -> Result<Target, crate::types::container::error::Error>
    where
        Self::Item: TryFit<<Target as IntoIterator>::Item>,
        Target: FromIterator<<Target as IntoIterator>::Item> + IntoIterator,
    {
        self.into_iter()
            .map(|x| x.try_fit())
            .collect::<Result<Target, _>>()
    }
}

impl<I: IntoIterator> RewrapExt for I {}
