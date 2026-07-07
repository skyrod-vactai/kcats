//! Support for containers in kcats. Includes types like [List],
//! [Set], [associative::Association], String, [pipe::In],
//! [pipe::Out], and Byte arrays. The container contract is you can
//! put things into, or take things out of them. [Receptacle]s are for
//! putting into, and [Dispenser]s are for taking out of. For
//! underlying types that support both operations (like [List]), we
//! can easily convert between [Receptacle] and [Dispenser] as needed.
pub mod associative;
pub mod dictionary;
pub mod environment;
pub mod error;
pub mod pipe;
pub mod program;
pub mod stack;

use self::associative as assoc;
use crate::derivation::*;
use crate::traits::*;
use crate::types::container::program::Program;
use crate::types::container::{error::Error, pipe::FutureTake};
use crate::types::number::Int;
use crate::types::*;
//use crate::{convert_enum, try_convert_enum};

use core::fmt;
use im;
use std::convert::Infallible;
use std::{collections::HashSet, sync};
use sync::Arc;

/// A generic List type
pub type Listy<I> = im::Vector<I>;

/// A generic Set type
pub type Setty<I> = HashSet<I>;

/// A specific List type
pub type List = Listy<Item>;
impl Identity for List {}

pub type Set = Arc<Setty<assoc::KeyItem>>;

impl Derive<HashSet<assoc::KeyItem>> for Set {
    fn derive(h: HashSet<assoc::KeyItem>) -> Set {
        Arc::new(h)
    }
}

impl Derive<List> for Item {
    fn derive(l: List) -> Item {
        Item::List(Box::new(l))
    }
}

// impl DeriveIterator<assoc::KeyItem> for Set {
//     fn derive_iter<I>(iter: I) -> Self
//     where
//         I: IntoIterator<Item = assoc::KeyItem>,
//     {
//         sync::Arc::new(iter.into_iter().collect::<HashSet<assoc::KeyItem>>())
//     }
// }

impl<T> Fresh for Arc<T>
where
    T: Default,
{
    fn fresh() -> Self {
        Arc::new(T::default())
    }
}

/// A trait for joining two values together. There are some precedence rules:
///
/// 1. If there are two different types being joined, the type that is
///    returned is either the most specialized types of the two being
///    joined, or the most specialized type that's possible to construct
///    given the two values. (For example, joining a Set with a List, or
///    vice versa, will always be a Set. Joining an Association with a
///    Dictionary will be an Associative enum but the variant will depend
///    on whether the Association data fits the schema of a
///    Dictionary. If so, it will be Dictionary, otherwise Assoc.)
///
/// 2. If the result type is keyed, (eg, Map or Set or struct types),
///    the RHS argument's keys take precedence over self's.
pub trait Join<RHS> {
    type Output;
    type Error;
    fn join(self, rhs: RHS) -> Result<Self::Output, Self::Error>;
}

impl Join<&str> for String {
    type Output = String;
    type Error = Infallible;
    fn join(mut self, rhs: &str) -> Result<Self::Output, Self::Error> {
        self.push_str(rhs);
        Ok(self)
    }
}

impl Join<char> for String {
    type Output = String;
    type Error = Infallible;
    fn join(mut self, rhs: char) -> Result<Self::Output, Self::Error> {
        self.push(rhs);
        Ok(self)
    }
}

impl Join<List> for List {
    type Output = List;
    type Error = Infallible;
    fn join(mut self, rhs: List) -> Result<Self::Output, Self::Error> {
        //println!("Joining list to list");
        self.extend(rhs.iter().cloned());
        Ok(self)
    }
}

impl Join<Set> for Set {
    type Output = Set;
    type Error = Infallible;
    fn join(mut self, rhs: Set) -> Result<Self::Output, Self::Error> {
        let am = self.mutate();
        am.extend(rhs.iter().cloned());
        Ok(self)
    }
}

/// When joining a List with a String, which type we get back depends
/// on the contents of the list. If the list has non-char items in it,
/// we get a List. Otherwise, a string.
impl Join<String> for List {
    type Output = Sized;
    type Error = Infallible;
    fn join(mut self, rhs: String) -> Result<Self::Output, Self::Error> {
        match String::try_derive(self.clone()) {
            Ok(mut s) => {
                s.push_str(rhs.as_str());
                Ok(Sized::String(s))
            }
            Err(_) => {
                // join as list
                self.extend(rhs.chars().map(Item::derive));
                Ok(Sized::List(Box::new(self)))
            }
        }
    }
}

/// When joining a String with a List, which type we get back depends
/// on the contents of the list. If the list has non-char items in it,
/// we get a List. Otherwise, a string.
impl Join<List> for String {
    type Output = Sized;
    type Error = Infallible;
    fn join(mut self, rhs: List) -> Result<Self::Output, Self::Error> {
        match String::try_derive(rhs.clone()) {
            Ok(s) => {
                self.push_str(s.as_str());
                Ok(Sized::String(self))
            }
            Err(_) => {
                // join as list
                let mut sl: List = self.fit();
                sl.extend(rhs.iter().cloned());
                Ok(Sized::List(Box::new(sl)))
            }
        }
    }
}

impl Join<List> for assoc::Associative {
    type Output = assoc::Associative;
    type Error = Error;
    fn join(self, other: List) -> Result<Self::Output, <Self as Join<List>>::Error> {
        //println!("Joining list to associative");
        let la = assoc::Associative::Assoc(std::sync::Arc::new(
            Sized::List(Box::new(other)).try_rewrap::<assoc::AssociationContent>()?,
        ));
        Ok(self.join(la).unwrap())
    }
}

impl Join<assoc::Associative> for List {
    type Output = assoc::Associative;
    type Error = Error;
    fn join(self, other: assoc::Associative) -> Result<Self::Output, Self::Error> {
        //println!("Joining associative to list");
        let sa =
            assoc::Associative::Assoc(Arc::new(self.try_rewrap::<assoc::AssociationContent>()?));
        Ok(sa.join(other).unwrap())
    }
}

/// Joining a List with a Set will be a set.
impl Join<Set> for List {
    type Output = Set;
    type Error = Error;
    fn join(self, mut other: Set) -> Result<Self::Output, Self::Error> {
        let bm = other.mutate();

        bm.extend(
            self.iter()
                .cloned()
                .map(assoc::KeyItem::try_derive)
                .collect::<Result<Vec<assoc::KeyItem>, Error>>()?,
        );
        Ok(other)
    }
}

impl Join<String> for String {
    type Output = String;
    type Error = Infallible;
    fn join(mut self, other: String) -> Result<Self::Output, Self::Error> {
        self.push_str(&other);
        Ok(self)
    }
}

/// Joins two containers into one.
impl Join<Sized> for Sized {
    type Output = Sized;
    type Error = Error;

    fn join(self, other: Sized) -> Result<Self::Output, Error> {
        //println!("Joining sized {:?} to sized {:?}", self, other);
        if self.is_empty() {
            return Ok(other);
        } else if other.is_empty() {
            return Ok(self);
        }

        Ok(match (self, other) {
            (Sized::Assoc(a), Sized::Assoc(b)) => Sized::Assoc(a.join(b).unwrap()),
            (Sized::DictEntry(a), Sized::DictEntry(b)) => {
                Sized::DictEntry(Box::new(a.join(*b).unwrap()))
            }
            (Sized::Error(a), Sized::Error(b)) => Sized::Error(a.join(b).unwrap()),
            (Sized::Words(a), Sized::Words(b)) => Sized::Words(a.join(b).unwrap()),
            (Sized::Words(a), Sized::List(b)) => {
                // maybe it's an assoc
                let ba: assoc::Association =
                    std::sync::Arc::new(Sized::List(b).try_rewrap::<assoc::AssociationContent>()?);
                a.join(ba).unwrap().fit()
            }
            (Sized::Env(a), Sized::Env(b)) => Sized::Env(Box::new(a.join(*b).unwrap())),
            (Sized::Dictionary(a), Sized::Dictionary(b)) => {
                a.join(*b).map(|d| Sized::Dictionary(Box::new(d))).unwrap()
            }

            (Sized::List(a), Sized::List(b)) => Sized::List(Box::new(a.join(*b).unwrap())),
            (Sized::List(a), Sized::Assoc(b)) => Sized::Assoc(a.join(b)?),
            (Sized::Assoc(a), Sized::List(b)) => Sized::Assoc(a.join(*b)?),

            (Sized::Set(a), Sized::Set(b)) => Sized::Set(a.join(b).unwrap()),
            (Sized::List(a), Sized::Set(b)) => Sized::Set(a.join(b)?),
            (Sized::Set(mut a), Sized::List(b)) => {
                let am = a.mutate();

                am.extend(
                    b.iter()
                        .cloned()
                        .map(assoc::KeyItem::try_derive)
                        .collect::<Result<Vec<assoc::KeyItem>, Error>>()?,
                );
                Sized::Set(a)
            }
(Sized::String(mut a), Sized::String(b)) => {
                a.push_str(&b);
                Sized::String(a)
            }
            (Sized::Bytes(mut a), Sized::Bytes(b)) => {
                a.extend(b);
                Sized::Bytes(a)
            }
            (Sized::String(s), Sized::List(l)) => s.join(*l).unwrap(),
            (Sized::List(l), Sized::String(s)) => l.join(s).unwrap(),
            (Sized::Program(mut a), Sized::Program(b)) => {
                a.clean();
                let mut b_prog = b.clone();
                b_prog.clean();
                if a.0.len() == 1 && b_prog.0.len() == 1 {
                    let mut a_chunk = (*a.0[0].chunk).clone();
                    let b_chunk = &b_prog.0[0].chunk;
                    if let Some(crate::types::container::program::Op::Return) = a_chunk.ops.last() {
                        a_chunk.ops.pop();
                    }
                    a_chunk.ops.extend(b_chunk.ops.iter().cloned());
                    a_chunk.source = match (a_chunk.source, &b_chunk.source) {
                        (Some(mut a_src), Some(b_src)) => {
                            a_src.extend(b_src.iter().cloned());
                            Some(a_src)
                        }
                        _ => None,
                    };
                    a.0[0].chunk = std::sync::Arc::new(a_chunk);
                    Sized::Program(a)
                } else {
                    let mut a_list = List::derive(a);
                    a_list.extend(List::derive(b_prog).iter().cloned());
                    Sized::List(Box::new(a_list))
                }
            }
            (Sized::Program(a), Sized::List(b)) => {
                let mut a_list = List::derive(a);
                a_list.extend(b.iter().cloned());
                Sized::List(Box::new(a_list))
            }
            (Sized::List(mut a), Sized::Program(b)) => {
                a.extend(List::derive(b).iter().cloned());
                Sized::List(a)
            }
            (s, other) => Err(Error::expected(fit!("joinable"), list!(s, other)))?,
        })
    }
}
pub trait Container<T> {
    fn has(&self, item: &T) -> bool;
}

pub trait Count {
    fn count(&self) -> usize;
}

/// A trait for containers where you can take an item out "in-place"
/// without blocking. The container itself is mutated and the item is
/// returned.
pub trait SimpleTake {
    type Item: Send + Fit<Item>;
    fn take_simple(&mut self) -> Option<Self::Item>;
}

// pub trait DemotingTake {
//     type Item;
//     type Output;
//     fn take_demoting(self) -> (Option<Self::Item>, Self::Output);
// }

pub trait Take {
    type Output;
    type Item;
    #[allow(clippy::type_complexity)]
    fn take(self) -> Sometime<'static, (Result<Option<Self::Item>, Error>, Self::Output)>;
}

/// A blanket impl for Take, for any type that already implements SimpleTake.
impl<T> Take for T
where
    T: SimpleTake + Send + 'static,
{
    type Output = T;
    type Item = Item;

    fn take(mut self) -> Sometime<'static, (Result<Option<Self::Item>, Error>, Self::Output)> {
        let item = self.take_simple();
        Sometime::Now((Ok(item.map(Fit::fit)), self))
    }
}

impl Count for Sized {
    fn count(&self) -> usize {
        match self {
            Self::Assoc(a) => a.len(),
            Self::DictEntry(a) => a.len(),
            Self::Env(e) => e.len(),
            Self::Error(e) => e.len(),
            Self::Words(d) => d.len(),
            Self::Dictionary(d) => d.len(),

            Self::List(l) => l.count(),
            Self::String(s) => s.len(),
            Self::Bytes(b) => b.len(),
            Self::Set(s) => s.len(),
            Self::Program(p) => p.count(),
        }
    }
}

impl Container<Item> for Sized {
    fn has(&self, other: &Item) -> bool {
        //println!("Has: {:?}\n{:?}", self, other);
        match (self, other) {
            (Sized::List(l), other) => l.contains(other),
            (Sized::Set(s), Item::Set(other)) => other.is_subset(s),
            (Sized::Set(s), other) => {
                assoc::KeyItem::try_derive(other.clone()).is_ok_and(|k| s.contains(&k))
            }
            (Sized::String(container), other) => match other {
                Item::Char(c) => container.has(c),
                i => match String::try_derive(i.clone()) {
                    Ok(ref s) => container.has(s),
                    Err(_) => false,
                },
            },
            (s, other) => {
                // (Sized::Associative(a), other) => {
                // assoc::KeyItem::try_derive(other.clone()).is_ok_and(|k| a.contains_key(&k))
                //}
                let c = assoc::Associative::try_derive(s.clone()).unwrap();
                assoc::KeyItem::try_derive(other.clone()).is_ok_and(|k| c.contains_key(&k))
            }
        }
    }
}

impl SimpleTake for Sized {
    //type Output = Self;
    type Item = Item;
    fn take_simple(&mut self) -> Option<Self::Item> {
        //println!("Taking! {:?}", self);
        match self {
            Sized::List(ref mut l) => l.take_simple(),
            Sized::String(ref mut s) => s.take_simple().map(Item::derive),
            Sized::Bytes(ref mut b) => b.take_simple().map(Item::derive),
            Sized::Set(ref mut s) => s.take_simple(),
            Sized::Program(ref mut p) => p.take_simple(),
            Sized::Assoc(ref mut a) => a.take_simple().map(Fit::fit),
            ref s => {
                let mut a = assoc::Associative::try_derive((*s).clone()).ok()?;

                let v = a.take_simple();
                *self = Sized::derive(a);
                v
            }
        }
    }
}

impl Count for String {
    fn count(&self) -> usize {
        self.len()
    }
}

impl Container<char> for String {
    fn has(&self, item: &char) -> bool {
        self.contains(*item)
    }
}

impl Container<String> for String {
    fn has(&self, item: &String) -> bool {
        self.contains(item.as_str())
    }
}

pub trait Ordered {
    /// Appends the items to the beginning of this list, preserving
    /// their order. eg `[1, 2, 3].append([4, 5, 6])` -> `[4, 5, 6, 1,
    /// 2, 3]`.
    fn prepend(&mut self, items: List);

    /// Appends the items in the iterator to the beginning of this
    /// list, preserving order.
    fn prepend_iter<T: IntoIterator<Item = Item>>(&mut self, items: T);

    /// Reverses the order of the list.
    fn reverse(&mut self);
}

pub trait Mutey<T> {
    fn mutate(&mut self) -> &mut T;
}

impl<T: Clone> Mutey<T> for Arc<T> {
    fn mutate(&mut self) -> &mut T {
        Arc::make_mut(self)
    }
}

impl Count for List {
    fn count(&self) -> usize {
        self.len()
    }
}

impl Container<Item> for List {
    fn has(&self, i: &Item) -> bool {
        self.contains(i)
    }
}

// impl Take for List {
//     type Item = Item;
//     type Output = List;
//     fn take(mut self) -> Future<(Self::Output, Result<Option<Item>, Error>)> {
//         let v = self.mutate().pop_front();
//         Box::pin(future::ready((self, Ok(v))))
//     }
// }

//impl Take for

impl Ordered for List {
    fn prepend(&mut self, mut items: List) {
        items.append(self.clone());
        *self = items;
    }

    fn prepend_iter<T: IntoIterator<Item = Item>>(&mut self, items: T) {
        let iv: im::Vector<Item> = items.into_iter().collect();
        self.prepend(iv)
    }

    fn reverse(&mut self) {
        let r = self.iter().cloned().rev().collect();
        *self = r;
    }
}

/// A generic container type, all we know is it can contain multiple
/// items. Includes things like lists, sets, and IO channels. Items
/// can be taken out.
#[derive(Clone, PartialEq)]
pub enum Dispenser {
    /// A container with a known number of items inside
    Sized(Sized),
    /// A pipe that dispenses an unknown number of items
    Out(pipe::Out),
    /// Similar to Out but also convertible to [Receptacle]
    Tunnel(pipe::Tunnel),
}

impl fmt::Debug for Dispenser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Dispenser::Sized(sized) => write!(f, "Sized{:?}", sized),
            Dispenser::Out(out) => write!(f, "{:?}", out),
            Dispenser::Tunnel(tunnel) => write!(f, "{:?}", tunnel),
        }
    }
}
/// A generic container type, all we know is it can contain multiple
/// items. Includes things like lists, sets, and IO channels. Items
/// can be put in.
#[derive(Clone, PartialEq)]
pub enum Receptacle {
    /// A container with a known number of items inside
    Sized(Sized),
    /// A pipe that can receive an arbitrary number of items
    In(pipe::In),
    /// Similar to In but also convertible to [Dispenser]
    Tunnel(pipe::Tunnel),
}

impl fmt::Debug for Receptacle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Receptacle::Sized(sized) => write!(f, "{:?}", sized),
            Receptacle::In(i) => write!(f, "{:?}", i),
            Receptacle::Tunnel(tunnel) => write!(f, "{:?}", tunnel),
        }
    }
}

/// Collections that have a definite size that we can access. Implies
/// that it can also be appended to.
#[derive(Clone)]
pub enum Sized {
    /// A generic associative structure where you can associate any
    /// [KeyItem] with any [Item].
    Assoc(assoc::Association),
    /// Represents an [dict::Dictionary] entry structure with
    /// specific keys.
    DictEntry(Box<dict::Entry>),
    /// Represents an execution environment, with specific keys
    Env(Box<env::Environment>),
    /// Represents a runtime Error value, with specific keys
    Error(Error),
    /// Represents the words available in to use
    Words(dict::Words),
    /// Represents a dictionary, including which modules have priority
    Dictionary(Box<dict::Dictionary>),

    /// List containers have multiple Items in a specific order.
    List(Box<List>),
    /// Program containers are optimized for execution of programs, but
    /// also act like Lists
    Program(Program),
    /// Set containers have multiple Items in no particular order, and
    /// each Item can only appear once.
    Set(Set),
    //TODO: these should be inside an Arc too
    /// A String is a chunk of text, like a list of individual
    /// characters.
    String(String),
    /// Bytes is the lowest common denominator form of data, useful
    /// for when no other type applies.
    Bytes(Bytes),
}

impl Identity for Sized {}

impl fmt::Debug for Sized {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sized::List(l) => write!(f, "List{:?}", l),
            Sized::Set(s) => write!(f, "{:?}", s),
            Sized::String(s) => write!(f, "{:?}", s),
            Sized::Bytes(b) => write!(f, "{:?}", b),
            Sized::Program(p) => write!(f, "{:?}", p),
            Sized::DictEntry(p) => write!(f, "{:?}", p),
            Sized::Dictionary(p) => write!(f, "{:?}", p),
            Sized::Env(p) => write!(f, "{:?}", p),
            Sized::Error(p) => write!(f, "{:?}", p),
            Sized::Assoc(p) => write!(f, "{:?}", p),
            Sized::Words(p) => write!(f, "{:?}", p),
        }
    }
}

/// Empty Sized containers are equal to each other.
impl PartialEq for Sized {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Sized::List(a), Sized::List(b)) => a == b,
            (Sized::String(a), Sized::String(b)) => a == b,
            (Sized::Bytes(a), Sized::Bytes(b)) => a == b,
            (Sized::Set(a), Sized::Set(b)) => a == b,
            (Sized::Assoc(a), Sized::Assoc(b)) => a == b,
            (Sized::DictEntry(a), Sized::DictEntry(b)) => a == b,
            (Sized::Env(a), Sized::Env(b)) => a == b,
            (Sized::Error(a), Sized::Error(b)) => a == b,
            (Sized::Dictionary(a), Sized::Dictionary(b)) => a == b,
            _ => self.is_empty() && other.is_empty(),
        }
    }
}

/// Takes an item out of the [Dispenser], and returns a future
/// that gives a new [Dispenser], and the [Item] that was removed
/// (if there was one).
impl Take for Dispenser {
    type Output = Self;
    type Item = Item;
    fn take(self) -> Sometime<'static, (Result<Option<Self::Item>, Error>, Self::Output)> {
        match self {
            Dispenser::Sized(mut s) => {
                let v = s.take_simple();
                //let (r, s) = s.take();
                // i.map(|r| {
                //     (Dispenser::SIzed(s), Self::result_to_option(r))
                // })

                Sometime::Now((Ok(v), Dispenser::Sized(s)))
            }
            Dispenser::Out(mut o) => Sometime::Future(Box::pin(async move {
                (o.take_future().await, Dispenser::Out(o))
            })),
            Dispenser::Tunnel(mut t) => Sometime::Future(Box::pin(async move {
                (t.take_future().await, Dispenser::Tunnel(t))
            })),
        }
    }
}

pub fn result_to_option(r: Result<Option<Item>, Error>) -> Option<Item> {
    match r {
        Ok(Some(i)) => Some(i),
        Ok(None) => None,
        Err(e) => Some(Item::derive(e)),
    }
}

impl Dispenser {
    // /// Takes an item out of the [Dispenser], and returns a future
    // /// that gives a new [Dispenser], and the [Item] that was removed
    // /// (if there was one).
    // pub fn take(self) -> Future<(Dispenser, Option<Item>)> {
    //     match self {
    //         Dispenser::Sized(mut s) => {
    //             let v = s.take_simple();
    //             //let (r, s) = s.take();
    //             // i.map(|r| {
    //             //     (Dispenser::SIzed(s), Self::result_to_option(r))
    //             // })

    //             Box::pin(future::ready((Dispenser::Sized(s), v)))
    //         }
    //         Dispenser::Out(mut o) => Box::pin({
    //             let i = o.take();
    //             i.map(|r| (Dispenser::Out(o), Self::result_to_option(r)))
    //         }),
    //         Dispenser::Tunnel(mut t) => Box::pin({
    //             let i = t.take();
    //             i.map(|r| {
    //                 (
    //                     Dispenser::Tunnel(t),
    //                     match r {
    //                         Ok(Some(i)) => Some(i),
    //                         Ok(None) => None,
    //                         Err(e) => Some(Item::derive(e)),
    //                     },
    //                 )
    //             })
    //         }),
    //     }
    // }
}

impl SimpleTake for List {
    type Item = Item;
    fn take_simple(&mut self) -> Option<Self::Item> {
        if self.is_empty() {
            None
        } else {
            self.pop_front()
        }
    }
}

impl SimpleTake for String {
    type Item = char;
    fn take_simple(&mut self) -> Option<Self::Item> {
        // TODO: this may perform badly
        let first_char = self.chars().next();
        self.drain(..first_char.map(|s| s.len_utf8()).unwrap_or(0));
        first_char
    }
}

impl SimpleTake for Bytes {
    type Item = Int;
    fn take_simple(&mut self) -> Option<Self::Item> {
        if self.is_empty() {
            None
        } else {
            let i = Some(self[0] as Int);
            self.drain(..1);
            i
        }
    }
}

impl SimpleTake for Set {
    type Item = Item;
    fn take_simple(&mut self) -> Option<Self::Item> {
        let sm = self.mutate();

        // First identify an item to remove (if any)
        let to_remove = match sm.iter().next() {
            Some(item) => item.clone(),
            None => return None,
        };

        // Remove the identified item
        sm.remove(&to_remove);

        // Return the removed item after applying fit()
        Some(to_remove.fit())
    }
}

impl Sized {
    /// Returns whether the container is empty
    pub fn is_empty(&self) -> bool {
        self.count() == 0
    }

    /// Takes an item from the back (end) of the container, if it is
    /// ordered, otherwise an arbitrary element.
    pub fn pop(self) -> (Self, Option<Item>) {
        match self {
            Sized::List(mut l) => {
                let i = l.pop_back();
                (Sized::List(l), i)
            }
            Sized::String(mut s) => s
                .pop()
                .map(|c| (Sized::String(s), Some(c.fit())))
                .unwrap_or((Sized::String(String::new()), None)),
            Sized::Bytes(mut b) => b
                .pop()
                .map(|c| (Sized::Bytes(b), Some((c as Int).fit())))
                .unwrap_or((Sized::Bytes(vec![]), None)),
            Sized::Set(mut s) => {
                let i = s.iter().next().cloned();
                let sm = s.mutate();
                if let Some(i) = i.clone() {
                    sm.take(&i);
                }
                (Sized::Set(s), i.map(Item::derive))
            }
            Sized::Program(mut p) => {
                let i = p.0.pop().map(|f| crate::types::Item::Program(Box::new(crate::types::container::program::Program(vec![f]))));
                (Sized::Program(p), i)
            }
            s => {
                // should be an associative
                let mut c = assoc::Associative::try_derive(s).unwrap();
                let x = c.take_simple();
                (Sized::derive(c), x)
            }
        }
    }

    /// Puts an item into the container, at the end.
    pub fn put(self, other: Item) -> Result<Self, Error> {
        match (self, other) {
            (Sized::List(mut c), i) => {
                c.push_back(i);
                Ok(Sized::List(c))
            }
            (Sized::Set(mut s), i) => {
                s.mutate().insert(assoc::KeyItem::try_derive(i)?);
                Ok(Sized::Set(s))
            }
            (Sized::Bytes(mut b), Item::Int(i)) => {
                b.push(i as u8);
                Ok(Sized::Bytes(b))
            }
            (Sized::Bytes(_), i) => Err(Error::expected(fit!("integer"), i)),
            (Sized::String(mut s), Item::Char(c)) => {
                s.push(c);
                Ok(Sized::String(s))
            }
            (Sized::String(_), i) => Err(Error::expected(fit!("char"), i)),
            (Sized::Program(mut p), i) => {
                p.extend(list![i]);
                Ok(Sized::Program(p))
            }
            (s, i) => {
                let c = assoc::Associative::try_derive(s.clone())?;
                Ok(c.put(i)?.fit())
            }
        }
    }

    /// Returns a new empty version of this container. Does not
    /// modify this container. The new container will be the same
    /// type as this one (if this is a [Sized::String], you'll get an empty
    /// [Sized::String], etc)
    pub fn empty(&self) -> Sized {
        match self {
            Sized::List(_) => Sized::List(Box::default()),
            Sized::Set(_) => Sized::Set(Set::default()),
            Sized::String(_) => Sized::String(String::new()),
            Sized::Bytes(_) => Sized::Bytes(vec![]),
            Sized::Program(_) => Sized::Program(Program::default()),
            // We assume it must be an associative at this point
            _ => Sized::Assoc(assoc::Association::fresh()),
        }
    }
}

impl Receptacle {
    /// Puts the given [Item] into this container, items are added at
    /// the end.
    pub fn put(self, i: Item) -> Sometime<'static, Result<Receptacle, Error>> {
        match self {
            Receptacle::Sized(s) => Sometime::Now(s.put(i).map(Receptacle::Sized)),
            Receptacle::In(p) => p.put(i).map(|r| r.map(Receptacle::In)),
            Receptacle::Tunnel(t) => t.put(i).map(|r| r.map(Receptacle::Tunnel)),
        }
    }
}

// these types are also not errors so mark them as such
impl OkItem for Sized {}
impl OkItem for List {}
impl OkItem for Receptacle {}
impl OkItem for Dispenser {}

impl IntoIterator for Sized {
    type Item = Item;
    type IntoIter = Box<dyn Iterator<Item = Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Sized::List(list) => {
                let items: Vec<_> = list.iter().cloned().collect();
                Box::new(items.into_iter())
            }
            Sized::String(s) => {
                let chars: Vec<char> = s.chars().collect();
                Box::new(chars.into_iter().map(|c| c.fit()))
            }
            Sized::Bytes(b) => {
                let vec: Vec<Item> = b
                    .into_iter()
                    .map(|byte| Item::derive(byte as Int))
                    .collect();
                Box::new(vec.into_iter())
            }
            Sized::Set(s) => {
                let items: Vec<_> = s.iter().cloned().map(|i| i.fit()).collect();
                Box::new(items.into_iter())
            }
            Sized::Program(p) => Box::new(List::derive(p).into_iter()),
            s => {
                // should be associative if we get here
                let map = assoc::Associative::try_derive(s).unwrap();
                Box::new(map.into_iter().map(|kv| kv.fit()))
            }
        }
    }
}

impl TryDerive<Dispenser> for Sized {
    fn try_derive(c: Dispenser) -> Result<Self, Error> {
        //println!("from iterable {:?}", c);
        match c {
            Dispenser::Sized(s) => Ok(s),
            i => Err(Error::expected(fit!("sized"), i)),
        }
    }
}

impl TryDerive<Receptacle> for Sized {
    fn try_derive(c: Receptacle) -> Result<Self, Error> {
        match c {
            Receptacle::Sized(s) => Ok(s),
            i => Err(Error::expected(fit!("sized"), i)),
        }
    }
}

impl TryDerive<Sized> for List {
    fn try_derive(s: Sized) -> Result<Self, Error> {
        match s {
            Sized::List(l) => Ok(*l),
            s => Ok(s.rewrap::<List, Item>()),
        }
    }
}

impl TryDerive<List> for Vec<dict::Namespace> {
    fn try_derive(l: List) -> Result<Self, Error> {
        l.try_rewrap::<Self>()
    }
}

impl Derive<Vec<Item>> for List {
    fn derive(v: Vec<Item>) -> Self {
        List::derive_iter(v)
    }
}

impl Derive<Vec<Item>> for Item {
    fn derive(v: Vec<Item>) -> Self {
        List::derive_iter(v).fit()
    }
}

impl Derive<String> for List {
    fn derive(s: String) -> Self {
        List::derive_iter(s.chars())
    }
}

impl TryDerive<List> for String {
    fn try_derive(l: List) -> Result<Self, Error> {
        let v = l
            .iter()
            .cloned()
            .map(Char::try_derive)
            .collect::<Result<Vec<Char>, Error>>()?;
        Ok(v.iter().collect::<String>())
    }
}

impl TryDerive<List> for Bytes {
    fn try_derive(l: List) -> Result<Self, Error> {
        let v = l
            .iter()
            .cloned()
            .map(|i| number::Int::try_derive(i).map(|i| i as u8))
            .collect::<Result<Bytes, Error>>()?;
        Ok(v)
    }
}

impl TryDerive<Item> for List {
    fn try_derive(i: Item) -> Result<Self, Error> {
        let s = Sized::try_derive(i)?;
        List::try_derive(s)
    }
}

impl TryDerive<Item> for Sized {
    fn try_derive(item: Item) -> Result<Self, Error> {
        match item {
            Item::Bytes(b) => Ok(Sized::Bytes(*b)),
            Item::String(s) => Ok(Sized::String(*s)),
            Item::List(l) => Ok(Sized::List(l)),
            Item::Assoc(a) => Ok(Sized::Assoc(a)),
            Item::Error(a) => Ok(Sized::Error(a)),
            Item::Dictionary(a) => Ok(Sized::Dictionary(a)),
            Item::DictEntry(a) => Ok(Sized::DictEntry(a)),
            Item::Words(a) => Ok(Sized::Words(a)),
            Item::Env(a) => Ok(Sized::Env(a)),
            Item::Set(s) => Ok(Sized::Set(s)),
            Item::Program(p) => Ok(Sized::Program(*p)),
            i => Err(Error::expected(fit!("sized"), i)),
            //try_convert_enum!(item, Item => Sized, [Bytes, String, List, Associative, Set, Program], |i| Self::NestedDeriveError::expected(fit!("sized"), i) )
        }
    }
}

impl TryDerive<Item> for Receptacle {
    fn try_derive(item: Item) -> Result<Self, Error> {
        match item {
            Item::StaticFile(f) => Ok(Receptacle::In(pipe::In::StaticFile(f))),
            Item::Socket(s) => Ok(Receptacle::In(pipe::In::Socket(s))),
            Item::Handoff(h) => Ok(Receptacle::In(pipe::In::Handoff(h))),
            Item::Standard => Ok(Receptacle::In(pipe::In::Standard)),
            Item::Bytes(b) => Ok(Receptacle::Sized(Sized::Bytes(*b))),
            Item::String(s) => Ok(Receptacle::Sized(Sized::String(*s))),
            Item::List(l) => Ok(Receptacle::Sized(Sized::List(l))),
            Item::Assoc(a) => Ok(Receptacle::Sized(Sized::Assoc(a))),
            Item::Error(a) => Ok(Receptacle::Sized(Sized::Error(a))),
            Item::Dictionary(a) => Ok(Receptacle::Sized(Sized::Dictionary(a))),
            Item::DictEntry(a) => Ok(Receptacle::Sized(Sized::DictEntry(a))),
            Item::Words(a) => Ok(Receptacle::Sized(Sized::Words(a))),
            Item::Env(a) => Ok(Receptacle::Sized(Sized::Env(a))),
            Item::Set(s) => Ok(Receptacle::Sized(Sized::Set(s))),
            Item::Program(p) => Ok(Receptacle::Sized(Sized::Program(*p))),
            i => Err(Error::expected(fit!("receptacle"), i)),
        }
    }
}

impl TryDerive<Dispenser> for Receptacle {
    fn try_derive(c: Dispenser) -> Result<Self, Error> {
        match c {
            Dispenser::Sized(s) => Ok(Receptacle::Sized(s)),
            Dispenser::Tunnel(t) => Ok(Receptacle::Tunnel(t)),
            i => Err(Error::expected(fit!("receptacle"), i)),
        }
    }
}

impl TryDerive<Receptacle> for Dispenser {
    fn try_derive(c: Receptacle) -> Result<Self, Error> {
        match c {
            Receptacle::Sized(s) => Ok(Dispenser::Sized(s)),
            Receptacle::Tunnel(t) => Ok(Dispenser::Tunnel(t)),
            i => Err(Error::expected(fit!("iterable"), i)),
        }
    }
}

impl TryDerive<Item> for Box<dyn Iterator<Item = Item>> {
    fn try_derive(item: Item) -> Result<Self, Error> {
        Ok(Sized::try_derive(item)?.into_iter())
    }
}

impl Derive<Sized> for Box<dyn Iterator<Item = Item>> {
    fn derive(sized: Sized) -> Self {
        Box::new(sized.into_iter())
    }
}

impl Derive<List> for Sized {
    fn derive(l: List) -> Self {
        Sized::List(Box::new(l))
    }
}

impl Derive<String> for Sized {
    fn derive(s: String) -> Self {
        Sized::String(s)
    }
}

impl Derive<Bytes> for Sized {
    fn derive(b: Bytes) -> Self {
        Sized::Bytes(b)
    }
}

impl Derive<Sized> for Dispenser {
    fn derive(s: Sized) -> Self {
        Dispenser::Sized(s)
    }
}

impl Derive<Set> for Item {
    fn derive(l: Set) -> Self {
        Item::Set(l)
    }
}

impl Derive<Dispenser> for Item {
    fn derive(d: Dispenser) -> Self {
        match d {
            Dispenser::Sized(Sized::Bytes(b)) => Item::Bytes(Box::new(b)),
            Dispenser::Sized(Sized::String(s)) => Item::String(Box::new(s)),
            Dispenser::Sized(Sized::List(l)) => Item::List(l),
            Dispenser::Sized(Sized::Set(s)) => Item::Set(s),
            Dispenser::Sized(Sized::Assoc(a)) => Item::Assoc(a),
            Dispenser::Sized(Sized::Dictionary(a)) => Item::Dictionary(a),
            Dispenser::Sized(Sized::DictEntry(a)) => Item::DictEntry(a),
            Dispenser::Sized(Sized::Words(a)) => Item::Words(a),
            Dispenser::Sized(Sized::Env(a)) => Item::Env(a),
            Dispenser::Sized(Sized::Error(a)) => Item::Error(a),
            Dispenser::Sized(Sized::Program(p)) => Item::Program(Box::new(p)),
            Dispenser::Out(o) => o.fit(),
            Dispenser::Tunnel(t) => t.fit(),
        }
    }
}

impl Derive<Receptacle> for Item {
    fn derive(r: Receptacle) -> Self {
        match r {
            Receptacle::Sized(Sized::Bytes(b)) => Item::Bytes(Box::new(b)),
            Receptacle::Sized(Sized::String(s)) => Item::String(Box::new(s)),
            Receptacle::Sized(Sized::List(l)) => Item::List(l),
            Receptacle::Sized(Sized::Set(s)) => Item::Set(s),
            Receptacle::Sized(Sized::Program(p)) => Item::Program(Box::new(p)),
            Receptacle::Sized(Sized::Assoc(a)) => Item::Assoc(a),
            Receptacle::Sized(Sized::Dictionary(a)) => Item::Dictionary(a),
            Receptacle::Sized(Sized::DictEntry(a)) => Item::DictEntry(a),
            Receptacle::Sized(Sized::Words(a)) => Item::Words(a),
            Receptacle::Sized(Sized::Env(a)) => Item::Env(a),
            Receptacle::Sized(Sized::Error(a)) => Item::Error(a),
            Receptacle::In(o) => o.fit(),
            Receptacle::Tunnel(t) => t.fit(),
        }
    }
}

impl Derive<Sized> for Item {
    fn derive(s: Sized) -> Self {
        Dispenser::Sized(s).fit()
    }
}

impl TryDerive<Item> for Dispenser {
    fn try_derive(item: Item) -> Result<Self, Error> {
        match item {
            Item::Bytes(b) => Ok(Dispenser::Sized(Sized::Bytes(*b))),
            Item::String(s) => Ok(Dispenser::Sized(Sized::String(*s))),
            Item::List(l) => Ok(Dispenser::Sized(Sized::List(l))),
            Item::Set(s) => Ok(Dispenser::Sized(Sized::Set(s))),
            Item::Assoc(a) => Ok(Dispenser::Sized(Sized::Assoc(a))),
            Item::Error(a) => Ok(Dispenser::Sized(Sized::Error(a))),
            Item::Dictionary(a) => Ok(Dispenser::Sized(Sized::Dictionary(a))),
            Item::DictEntry(a) => Ok(Dispenser::Sized(Sized::DictEntry(a))),
            Item::Words(a) => Ok(Dispenser::Sized(Sized::Words(a))),
            Item::Env(a) => Ok(Dispenser::Sized(Sized::Env(a))),
            Item::Nothing => Ok(Dispenser::Sized(Sized::default())),
            Item::Program(p) => Ok(Dispenser::Sized(Sized::Program(*p))),
            Item::StaticFile(f) => Ok(Dispenser::Out(pipe::Out::StaticFile(f))),
            Item::Socket(f) => Ok(Dispenser::Out(pipe::Out::Socket(f))),
            Item::ServerSocket(f) => Ok(Dispenser::Out(pipe::Out::ServerSocket(f))),
            Item::Handoff(f) => Ok(Dispenser::Out(pipe::Out::Handoff(f))),
            Item::Timer(f) => Ok(Dispenser::Out(pipe::Out::Timer(f))),
            Item::Standard => Ok(Dispenser::Out(pipe::Out::Standard)),
            Item::Time => Ok(Dispenser::Out(pipe::Out::Time)),

            i => Err(Error::expected(fit!("iterable"), i)),
        }
    }
}

impl TryDerive<Item> for Set {
    fn try_derive(item: Item) -> Result<Self, Error> {
        let s = Sized::try_derive(item)?;
        s.try_rewrap::<HashSet<assoc::KeyItem>>().map(Arc::new)
    }
}

impl Default for Sized {
    fn default() -> Self {
        Sized::List(Box::default())
    }
}

impl Default for Dispenser {
    fn default() -> Self {
        Dispenser::Sized(Sized::default())
    }
}

impl Default for Receptacle {
    fn default() -> Self {
        Receptacle::Sized(Sized::default())
    }
}

impl IntoList for Vec<Item> {}

mod serde {
    use super::{Dispenser, List, Receptacle, Sized};
    use crate::derivation::*;
    use crate::serialize::Display;
    use crate::types::container::associative as assoc;
    use serde::ser::{Serialize, SerializeMap, SerializeSeq};

    impl Serialize for Dispenser {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            match self {
                Dispenser::Out(o) => o.representation().serialize(serializer),
                Dispenser::Tunnel(t) => t.representation().serialize(serializer),
                Dispenser::Sized(s) => s.serialize(serializer),
            }
        }
    }

    impl Serialize for Receptacle {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            match self {
                Receptacle::In(i) => i.representation().serialize(serializer),
                Receptacle::Tunnel(t) => t.representation().serialize(serializer),
                Receptacle::Sized(s) => s.serialize(serializer),
            }
        }
    }

    impl Serialize for Sized {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            match self {
                Sized::List(ref l) => {
                    // Serialize a list (sequence)
                    let mut seq = serializer.serialize_seq(Some(l.len()))?;
                    for element in l.iter() {
                        seq.serialize_element(&element)?;
                    }
                    seq.end()
                }

                Sized::Program(p) => {
                    let l = List::derive(p.clone());
                    // Serialize a list (sequence)
                    let mut seq = serializer.serialize_seq(Some(l.len()))?;
                    for element in l.iter() {
                        seq.serialize_element(&element)?;
                    }
                    seq.end()
                }
                Sized::Bytes(b) => serializer.serialize_bytes(b.as_slice()),
                Sized::Set(s) => {
                    // Serialize a list (sequence)
                    let mut seq = serializer.serialize_seq(Some(s.len()))?;
                    for element in s.iter() {
                        seq.serialize_element(&element)?;
                    }
                    seq.end()
                }
                Sized::String(s) => serializer.serialize_str(s.as_str()),
                s => {
                    let assoc = assoc::Associative::try_derive(s.clone()).unwrap();
                    // Start serializing a map
                    let assoc = assoc::Association::derive(assoc);
                    let mut map = serializer.serialize_map(Some(assoc.len()))?;
                    for (key, value) in assoc.iter() {
                        // Serialize each entry in the map
                        map.serialize_entry(&key, &value)?;
                    }
                    // Finish serializing the map
                    map.end()
                }
            }
        }
    }
}
