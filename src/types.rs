//! Defines kcats internal data types.
use crate::axiom;
use crate::derivation::*;
use crate::serialize::{self, Emit};
use crate::types::coll::associative as assoc;
use crate::types::coll::Set;
use crate::types::container as coll;
use crate::types::container::dictionary as dict;
use crate::types::container::environment as env;
use crate::types::container::error::{Error, OkItem};
use crate::types::container::pipe::{channel, fs, net};
use crate::types::container::program::Program;
use crate::{fit, list};
use core::default::Default;
use core::fmt;
use futures::future::Either;
use futures::future::FutureExt;
use im;
use internment::Intern;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::future::{self, IntoFuture};
use std::hash::Hash;
use std::marker::Sync;

use std::sync::Arc;
use tokio::sync::RwLock;

pub mod container;
pub mod number;

/// A Word causes a kcats program to do something, usually taking some
/// items derive the top of the stack, and using them to create new
/// stack items. (examples: `swap`, `+`, `dip`).
#[derive(Clone, Eq, PartialOrd, Ord, Default, Hash, PartialEq, Debug)]
pub struct WordData {
    pub data: Intern<String>,
    pub quoted: bool,
    pub namespace: Option<dict::Namespace>,
}

pub type Word = Intern<WordData>;

impl TryDerive<String> for Word {
    fn try_derive(s: String) -> Result<Self, Error> {
        let original_s = s.clone();
        match serialize::parse(s.clone()) {
            Ok(mut l) => {
                if l.len() == 1 {
                    match l.pop_front().unwrap() {
                        Item::Word(w) => Ok(w),
                        _ => Err(Error::expected(
                            fit!("word"),
                            Item::String(Box::new(original_s)),
                        )),
                    }
                } else if l.is_empty() {
                    Err(Error::parse("Words cannot be empty"))
                } else {
                    Err(Error::parse("Words cannot contain whitespace"))
                }
            }
            Err(e) => Err(e),
        }
    }
}

impl TryCast<String> for Word {
    fn try_cast(s: String) -> Result<Self, String> {
        // Fast conversion: if it fails, return the original string
        match serialize::parse(s.clone()) {
            Ok(mut l) => {
                if l.len() == 1 {
                    match l.pop_front().unwrap() {
                        Item::Word(w) => Ok(w),
                        _ => Err(s),
                    }
                } else {
                    Err(s)
                }
            }
            Err(_) => Err(s),
        }
    }
}

/// A macro for creating Word Items from literal strings in code,
/// panics if the strings are not valid Words. But that should happen
/// at compile time because it can only be used on literals.
#[macro_export] // make it usable from other crates too
macro_rules! fit {
    // accept exactly one literal, with an optional trailing comma
    ($lit:literal $(,)?) => {{
        // bring the trait into scope so the method call is found
        use $crate::derivation::TryFit as _;

        // do what `"literal".try_fit().unwrap()` used to do
        ($lit).try_fit().unwrap()
    }};
}

/// A macro to convert from one enum type to another (assuming variant names match.)
/// enum Foo {
///     Var(i32),
///     Other(String),
///     Another(bool),
/// }
///
/// enum Bar {
///     Var(i32),
///     Other(String),
///     Another(bool),
/// }
///
/// fn main() {
///     let foo = Foo::Var(42);
///     let bar = convert_enum!(foo, Foo, Bar, [Var, Other, Another]);
/// }
#[macro_export]
macro_rules! convert_enum {
    ($value:expr, $from:ident, $to:ident, [$($variant:ident),* $(,)?] $(, $pattern:pat => $result:expr)* $(,)?) => {{
        use $from::*;
        match $value {
            $(
                $variant(inner) => $to::$variant(inner),
            )*
            $(
                $pattern => $result,
            )*
        }
    }};
}

/// Fallible: source → subset target (some variants may not exist)
#[macro_export]
macro_rules! try_convert_enum {
    ($value:expr, $from:ident => $to:ident, [$($variant:ident),* $(,)?], $err:expr) => {{
        use $from::*;
        match $value {
            $(
                $variant(inner) => Ok($to::$variant(inner)),
            )*
            other => Err($err(other)),
        }
    }};
}

impl TryDerive<&str> for Word {
    fn try_derive(s: &str) -> Result<Self, Error> {
        Word::try_derive(s.to_string())
    }
}

impl TryCast<&str> for Word {
    fn try_cast(s: &str) -> Result<Self, &str> {
        Word::try_cast(s.to_string()).map_err(|_| s)
    }
}

impl<'a> Derive<&'a Word> for &'a str {
    fn derive(s: &'a Word) -> Self {
        s.data.as_str()
    }
}

impl Derive<Word> for String {
    fn derive(s: Word) -> Self {
        s.data.to_string()
    }
}

impl WordData {
    /// Strips prefix from the data part of the word (leaves namespace
    /// unaffected). Works just like the String version of this method
    pub fn strip_prefix(&self, prefix: &str) -> Option<Word> {
        self.data.strip_prefix(prefix).map(|d| {
            Intern::new(WordData {
                data: Intern::new(d.to_string()),
                namespace: self.namespace,
                quoted: self.quoted,
            })
        })
    }
}

/// A byte array type
pub type Bytes = Vec<u8>;

/// A character type
pub type Char = char;

// Some static values for commonly used words
lazy_static! {
    pub static ref S_ASSOC: Word = fit!("association");
    pub static ref S_BOOLEAN: Word = fit!("boolean");
    pub static ref S_BYTES: Word = fit!("bytes");
    pub static ref S_CHAR: Word = fit!("character");
    pub static ref S_DICTIONARY: Word = fit!("dictionary");
    pub static ref S_DISPENSER: Word = fit!("dispenser");
    pub static ref S_ENVIRONMENT: Word = fit!("environment");
    pub static ref S_ERROR: Word = fit!("error");
    pub static ref S_FLOAT: Word = fit!("float");
    pub static ref S_INTEGER: Word = fit!("integer");
    pub static ref S_ITEM: Word = fit!("item");
    pub static ref S_LIST: Word = fit!("list");
    pub static ref S_NUMBER: Word = fit!("number");
    pub static ref S_ORDERED: Word = fit!("ordered");
    pub static ref S_PIPE: Word = fit!("pipe");
    pub static ref S_PROGRAM: Word = fit!("program");
    pub static ref S_RECEPTACLE: Word = fit!("receptacle");
    pub static ref S_SIZED: Word = fit!("sized");
    pub static ref S_STRING: Word = fit!("string");
    pub static ref S_WORD: Word = fit!("word");
    pub static ref S_SET: Word = fit!("set");
}

use std::any::TypeId;

/// A function to give the name of a type, useful for specs and errors
pub fn get_name<T: 'static>() -> &'static Word {
    match TypeId::of::<T>() {
        t if t == TypeId::of::<Vec<u8>>() => &S_BYTES,
        t if t == TypeId::of::<String>() || t == TypeId::of::<&str>() => &S_STRING,
        t if t == TypeId::of::<Word>() => &S_WORD,
        t if t == TypeId::of::<coll::List>() => &S_LIST,
        t if t == TypeId::of::<Item>() => &S_ITEM,
        t if t == TypeId::of::<coll::Set>() => &S_SET,
        t if t == TypeId::of::<coll::Dispenser>() => &S_DISPENSER,
        t if t == TypeId::of::<coll::Receptacle>() => &S_RECEPTACLE,
        t if t == TypeId::of::<coll::Sized>() => &S_SIZED,
        t if t == TypeId::of::<assoc::Associative>() || t == TypeId::of::<assoc::Association>() => {
            &S_ASSOC
        }
        t if t == TypeId::of::<Error>() => &S_ERROR,
        t if t == TypeId::of::<env::Environment>() => &S_ENVIRONMENT,
        t if t == TypeId::of::<dict::Dictionary>() => &S_DICTIONARY,
        t if t == TypeId::of::<number::Number>() => &S_NUMBER,
        t if t == TypeId::of::<Char>() => &S_CHAR,
        _ => &S_ITEM,
    }
}

/// A kcats data value.
#[derive(Clone)]
pub enum Item {
    /// A number value
    Int(number::Int),
    Float(number::Float),

    /// A word value. Words are atomic, they can't be broken down into
    /// characters like Strings.
    Word(Word),
    /// A character value, like 'a', or '\n'.
    Char(Char),
    /// A builtin function
    Builtin(Box<axiom::Builtin>),
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
    /// A memory efficient representation of empty container
    Nothing,

    /// List containers have multiple Items in a specific order.
    List(Box<coll::List>),
    /// Program containers are optimized for execution of programs, but
    /// also act like Lists
    Program(Box<Program>),
    /// Set containers have multiple Items in no particular order, and
    /// each Item can only appear once.
    Set(Set),
    //TODO: these should be inside an Arc too
    /// A String is a chunk of text, like a list of individual
    /// characters.
    String(Box<String>),
    /// Bytes is the lowest common denominator form of data, useful
    /// for when no other type applies.
    Bytes(Box<Bytes>),
    //Out(pipe::Out),
    /// Pipe types
    /// A pipe that produces bytes from a file on disk
    StaticFile(Arc<RwLock<fs::StaticFile>>),
    /// A pipe that produces bytes from a TCP/IP socket
    Socket(Arc<RwLock<net::Socket>>),
    /// A pipe that produces sockets from a TCP/IP server socket
    ServerSocket(Arc<RwLock<net::ServerSocket>>),
    /// A pipe that produces items from a channel that comes from
    /// another part of the program
    Handoff(Box<channel::Handoff<Item>>),
    /// A pipe that produces a dummy value after a given amount of
    /// time. Can be used as a timeout mechanism when waiting on
    /// multiple pipes at once.
    Timer(Box<channel::Timer>),
    /// A pipe that produces timestamps of the current UNIX time
    Time,
    /// A pipe that produces bytes from standard in.
    Standard,
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Char(c) => write!(f, "Char{:?}", c),
            Item::Builtin(b) => write!(f, "Builtin{:?}", b.name),
            Item::Int(i) => write!(f, "{:?}", i),

            Item::Float(fl) => write!(f, "{:?}", fl),
            Item::Bytes(s) => write!(f, "{:?}", s),
            Item::List(s) => write!(f, "{:?}", s),
            Item::Set(s) => write!(f, "{:?}", s),
            Item::Program(s) => write!(f, "{:?}", s),
            Item::Assoc(s) => write!(f, "{:?}", s),
            Item::DictEntry(s) => write!(f, "{:?}", s),
            Item::Env(s) => write!(f, "{:?}", s),
            Item::Error(s) => write!(f, "{:?}", s),
            Item::Words(s) => write!(f, "{:?}", s),
            Item::Dictionary(s) => write!(f, "{:?}", s),
            Item::Nothing => write!(f, "Nothing!"),
            Item::String(s) => write!(f, "{:?}", s),

            Item::Handoff(h) => write!(f, "{:?}", h),
            Item::Socket(s) => write!(f, "{:?}", s),
            Item::ServerSocket(s) => write!(f, "{:?}", s),
            Item::StaticFile(ff) => write!(f, "{:?}", ff),
            Item::Timer(t) => write!(f, "{:?}", t),
            Item::Standard => write!(f, "Standard"),
            Item::Time => write!(f, "Time"),
            Item::Word(w) => {
                if w.quoted {
                    write!(f, "[{}]", Item::Word(*w).emit())
                } else {
                    write!(f, "{}", Item::Word(*w).emit())
                }
            }
        }
    }
}

impl Item {
    /// Returns whether the item is empty - only containers can be empty.
    pub fn is_empty(&self) -> bool {
        match self {
            Item::Bytes(b) => b.is_empty(),
            Item::String(s) => s.is_empty(),
            Item::List(l) => l.is_empty(),
            Item::Assoc(a) => a.is_empty(),
            Item::Words(a) => a.is_empty(),
            Item::Set(s) => s.is_empty(),
            Item::Program(p) => p.is_empty(),
            Item::Nothing => true,
            _ => false,
            //try_convert_enum!(item, Item => Sized, [Bytes, String, List, Associative, Set, Program], |i| Self::DeriveError::expected(fit!("sized"), i) )
        }
    }
}

/// A Future value, used for async execution, which is how
/// multithreading is implemented in kcats.
pub type Future<'a, T> = Pin<Box<dyn std::future::Future<Output = T> + Send + 'a>>;
pub enum Sometime<'a, T> {
    Now(T),
    Future(Future<'a, T>),
}

impl<'a, T> Sometime<'a, T> {
    pub fn map<U, F>(self, op: F) -> Sometime<'a, U>
    where
        F: FnOnce(T) -> U + Send + 'static,
        T: 'static,
    {
        match self {
            Sometime::Now(t) => Sometime::Now(op(t)),
            Sometime::Future(t) => Sometime::Future(Box::pin(t.map(op))),
        }
    }
}

impl<'a, T> IntoFuture for Sometime<'a, T> {
    type Output = T;
    // Either is a utility enum that implements Future
    // if both Left and Right enable it.
    // It avoids allocating a Box for the 'Now' case!
    type IntoFuture = Either<
        future::Ready<T>,                                          // Left: The fast path
        Pin<Box<dyn std::future::Future<Output = T> + Send + 'a>>, // Right: The slow path
    >;

    fn into_future(self) -> Self::IntoFuture {
        match self {
            // Fast path: Zero allocation.
            // We just wrap the value in a Ready struct.
            Sometime::Now(val) => Either::Left(future::ready(val)),

            // Slow path: Pass the existing Box through.
            Sometime::Future(fut) => Either::Right(fut),
        }
    }
}

/// A type for a function that advances the execution of a kcats
/// [env::Environment] by one step.




use std::pin::Pin;
pub enum StepResult {
    Done,
    Async(Pin<Box<dyn std::future::Future<Output = env::Environment> + Send + 'static>>),
}

pub type StepFn = dyn Fn(&mut env::Environment) -> StepResult + Sync + Send;

impl PartialEq for Item {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // same types, just use their own eq
            (Item::Int(a), Item::Int(b)) => a == b,
            (Item::Float(a), Item::Float(b)) => a == b,
            (Item::Float(a), Item::Int(b)) => *a == *b as number::Float,
            (Item::Int(a), Item::Float(b)) => *a as number::Float == *b,

            (Item::Word(a), Item::Word(b)) => a == b,
            (Item::Bytes(a), Item::Bytes(b)) => a == b,
            (Item::Program(a), Item::Program(b)) => a == b,
            (Item::Program(a), Item::List(b)) => coll::List::derive((**a).clone()) == **b,

            (Item::String(a), Item::String(b)) => a == b,
            (Item::List(a), Item::List(b)) => a == b,
            (Item::Set(a), Item::Set(b)) => a == b,
            (Item::Assoc(a), Item::Assoc(b)) => a == b,

            (Item::DictEntry(a), Item::DictEntry(b)) => a == b,
            (Item::Env(a), Item::Env(b)) => a == b,
            (Item::Error(a), Item::Error(b)) => a == b,
            (Item::Dictionary(a), Item::Dictionary(b)) => a == b,
            (Item::Nothing, Item::Nothing) => true,

            (Item::StaticFile(s1), Item::StaticFile(s2)) => Arc::ptr_eq(s1, s2),
            (Item::Socket(s1), Item::Socket(s2)) => Arc::ptr_eq(s1, s2),
            (Item::ServerSocket(s1), Item::ServerSocket(s2)) => Arc::ptr_eq(s1, s2),
            (Item::Handoff(h1), Item::Handoff(h2)) => h1 == h2,
            (Item::Time, Item::Time) => true,
            (Item::Standard, Item::Standard) => true,

            (Item::Char(a), Item::Char(b)) => a == b,
            (a, b) => a.is_empty() && b.is_empty(),
        }
    }
}

/// The default Item is empty list.
impl Default for Item {
    fn default() -> Self {
        coll::Dispenser::default().fit()
    }
}

impl TryDerive<Item> for String {
    fn try_derive(value: Item) -> Result<Self, Error> {
        match value {
            Item::String(s) => Ok(*s),
            Item::Word(w) => Ok(w.data.as_str().to_string()),
            i => {
                let s = coll::Sized::try_derive(i.clone())?;
                match s {
                    coll::Sized::String(s) => Ok(s),
                    _ => Err(Error::expected(fit!("string"), i)),
                }
            }
        }
    }
}

impl TryCast<Item> for String {
    fn try_cast(value: Item) -> Result<Self, Item> {
        match value {
            Item::String(s) => Ok(*s),
            i => Err(i),
        }
    }
}

/// Converts Item to Word but also considers a quoted word as a word,
/// eg \[foo\] -> foo.
impl TryDerive<Item> for Word {
    fn try_derive(value: Item) -> Result<Self, Error> {
        match value {
            Item::Word(i) => Ok(i),
            i => {
                let s = coll::Sized::try_derive(i.clone())?;
                match s {
                    coll::Sized::String(s) => s
                        .try_fit()
                        .map_err(|_| Error::expected(fit!("word"), i.clone())),
                    s => {
                        let l = coll::List::try_derive(s);
                        match l {
                            Ok(mut l) => {
                                if l.len() == 1 {
                                    let i = l.pop_front().unwrap();
                                    i.try_fit()
                                } else {
                                    Err(Error::expected(fit!("word"), i))
                                }
                            }
                            Err(_) => Err(Error::expected(fit!("word"), i)),
                        }
                    }
                }
            }
        }
    }
}

impl TryDerive<Item> for Bytes {
    fn try_derive(value: Item) -> Result<Self, Error> {
        let s = coll::Sized::try_derive(value)?;
        match s {
            coll::Sized::Bytes(b) => Ok(b),
            b => Err(Error::expected(fit!("bytes"), b)),
        }
    }
}

impl TryDerive<Item> for char {
    fn try_derive(value: Item) -> Result<Self, Error> {
        match value {
            Item::Char(c) => Ok(c),
            Item::Int(i) => match char::from_u32(i as u32) {
                Some(c) => Ok(c),
                None => Err(Error::expected(fit!("character-encoding"), Item::Int(i))),
            },
            b => Err(Error::expected(fit!("character"), b)),
        }
    }
}

impl TryCast<Item> for char {
    fn try_cast(value: Item) -> Result<Self, Item> {
        match value {
            Item::Char(c) => Ok(c),
            i => Err(i),
        }
    }
}

/// As there are no real booleans, we use the word '✅' but literally
/// any value except empty containers is truthy. If we read a value
/// 'false', that's not actually a boolean, it's just the [Word]
/// false. The fact that the word '✅' is used in the language but
/// 'no' is not, is a known tradeoff.
impl Derive<bool> for Item {
    fn derive(b: bool) -> Item {
        if b {
            fit!("✅")
        } else {
            Item::default()
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Error {
        Error::create(list!("io"), &err.to_string(), Option::<Item>::None)
    }
}

/// This impl is specifically for Word items, so it can fail
impl TryDerive<&str> for Item {
    fn try_derive(i: &str) -> Result<Self, Error> {
        Ok(Item::Word(Word::try_derive(i)?))
    }
}

impl Derive<String> for Item {
    fn derive(i: String) -> Self {
        Item::String(Box::new(i))
    }
}

impl Derive<Bytes> for Item {
    fn derive(b: Bytes) -> Self {
        Item::Bytes(Box::new(b))
    }
}

impl Derive<Word> for Item {
    fn derive(w: Word) -> Self {
        Item::Word(w)
    }
}

impl Derive<WordData> for Item {
    fn derive(w: WordData) -> Self {
        Item::Word(Intern::new(w))
    }
}

impl Derive<WordData> for Word {
    fn derive(w: WordData) -> Self {
        Intern::new(w)
    }
}

impl Derive<Char> for Item {
    fn derive(c: Char) -> Self {
        Item::Char(c)
    }
}

impl Derive<()> for Item {
    fn derive(_: ()) -> Self {
        Item::default()
    }
}

impl<T> Derive<Option<T>> for Item
where
    Item: Derive<T>,
{
    fn derive(opt: Option<T>) -> Item {
        match opt {
            Some(t) => Item::derive(t),
            None => Item::default(),
        }
    }
}

/// Convert reference to Item to Item, by cloning, useful when the
/// reference is buried in other structures that can handle Fit<Item>s.
impl Derive<&Item> for Item {
    fn derive(i: &Item) -> Item {
        i.clone()
    }
}

/// Try to convert any reference to Item to T, and don't destroy the
/// original in case it fails (so the caller could try again if
/// needed.)
impl<T: TryDerive<Item>> TryDerive<&Item> for T
where
    T: TryDerive<Item>,
{
    fn try_derive(value: &Item) -> Result<Self, Error> {
        T::try_derive(value.clone())
    }
}

/// We aren't doing a blanket T=>T identity conversion because it
/// conflicts with other useful impls. We limit it to types marked
/// with Identity trait.
impl Identity for Item {}
impl Identity for String {}
impl Identity for Word {}
impl Identity for Bytes {}

// these types are also not errors so mark them as such
impl OkItem for Item {}
impl OkItem for String {}
impl OkItem for Word {}
impl OkItem for Bytes {}

/// A generic impl to convert an Item to a vec of the given
/// type. Assumes the Item is some sort of container and converts each
/// item in the container.
impl<T: TryDerive<Item>> TryDerive<Item> for Vec<T> {
    fn try_derive(value: Item) -> Result<Self, Error> {
        // First try to convert the Item to an IntoIterator<Item>
        let it: Box<dyn Iterator<Item = Item>> = value.try_fit()?;
        it.map(T::try_derive).collect()
    }
}

impl<T: TryCast<Item>> TryCast<Item> for Vec<T> {
    fn try_cast(value: Item) -> Result<Self, Item> {
        let original = value.clone();
        let it: Result<Box<dyn Iterator<Item = Item>>, _> = TryFit::try_fit(value);
        if let Ok(it) = it {
            let res: Result<Vec<T>, Item> = it
                .map(|item| {
                    T::try_cast(item).map_err(|_| Item::Word(Word::default())) // Placeholder failure
                })
                .collect();
            res.map_err(|_| original)
        } else {
            Err(original)
        }
    }
}

/// A macro to build a kcats List, accepts any values that are
/// convertible to [Item].
#[macro_export]
macro_rules! list {
    /* public entry point -------------------------------------------------- */
    ( $( $elem:expr ),* $(,)? ) => {{
        //use $crate::traits::{Fit, TryFit as _};   // methods in scope
        use $crate::types::{Item, container::List};
        #[allow(unused_imports)]
        use $crate::derivation::{Derive, Fit, TryFit};
        // turn every element into an Item, one by one
        let v: Vec<Item> = vec![
            $( list!(@conv $elem) ,)*
        ];

        List::derive(v)
    }};

    /* helper: convert one element ----------------------------------------- */

    /* literal → try_fit().unwrap() */
    (@conv $lit:literal) => {
        ($lit).try_fit().unwrap()
    };

    /* anything else → fit() */
    (@conv $expr:expr) => {
        ($expr).fit()
    };
}

mod serde {
    //! Support for json serialization of kcats objects
    use super::Item;
    use crate::derivation::*;
    use crate::serialize::Display;
    use crate::types::container as coll;
    use crate::types::container::{
        associative as assoc,
        pipe::{standard, time},
    };

    use crate::types::Error;
    use futures::executor;
    use serde::de::{self, Deserialize, Deserializer, Visitor};
    use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};
    use std::collections::HashMap;
    use std::fmt;

    struct ItemVisitor;

    impl<'de> Visitor<'de> for ItemVisitor {
        type Value = Item;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("expected a specific representation for Item")
        }

        fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Item::Int(value))
        }

        fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Item::Int(value as i64))
        }

        fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Item::Float(value))
        }

        fn visit_none<E>(self) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Item::default())
        }

        fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Item::derive(v))
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Item::String(Box::new(v.to_string())))
        }

        fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Item::Bytes(Box::new(v)))
        }

        fn visit_map<A>(self, mut ma: A) -> Result<Self::Value, A::Error>
        where
            A: de::MapAccess<'de>,
        {
            let mut map = HashMap::new();
            while let Some((key, value)) = ma.next_entry::<assoc::KeyItem, Item>()? {
                map.insert(key, value);
            }
            Ok(Item::Assoc(map.fit()))
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: de::SeqAccess<'de>,
        {
            let mut items = Vec::new();
            while let Some(item) = seq.next_element::<Item>()? {
                items.push(item);
            }
            Ok(coll::List::derive(items).fit())
        }
    }

    impl<'de> Deserialize<'de> for Item {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_any(ItemVisitor)
        }
    }

    impl From<serde_json::Error> for Error {
        fn from(err: serde_json::Error) -> Error {
            Error::create(list!("serialize"), &err.to_string(), Option::<Item>::None)
        }
    }

    impl Serialize for Item {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            fn ser_map<S: Serializer>(
                s: S,
                m: Box<dyn Iterator<Item = assoc::Entry>>,
            ) -> Result<S::Ok, S::Error> {
                let mut map = s.serialize_map(None)?;
                for (key, value) in m {
                    // Serialize each entry in the map
                    map.serialize_entry(&key, &value)?;
                }
                // Finish serializing the map
                map.end()
            }

            match self {
                Item::Int(num) => num.serialize(serializer),
                Item::Float(num) => num.serialize(serializer),
                Item::Char(c) => serializer.serialize_char(*c),
                Item::Word(w) => serializer.serialize_str(w.fit()),
                Item::Builtin(b) => {
                    serializer.serialize_str(format!("builtin_{}", b.name).as_str())
                }
                // Handle other variants
                Item::Assoc(a) => ser_map(serializer, Box::new(a.as_ref().clone().into_iter())),
                Item::DictEntry(d) => ser_map(serializer, Box::new(d.clone().into_iter())),
                Item::Dictionary(d) => ser_map(serializer, Box::new(d.clone().into_iter())),
                Item::Error(e) => ser_map(serializer, Box::new(e.clone().into_iter())),
                Item::Env(e) => ser_map(serializer, e.clone().into_iter()),
                Item::Words(w) => ser_map(
                    serializer,
                    Box::new(
                        w.as_ref()
                            .clone()
                            .into_iter()
                            .map(|(k, v)| (k.fit(), v.fit())),
                    ),
                ),
                Item::Nothing => {
                    let seq = serializer.serialize_seq(Some(0))?;
                    seq.end()
                }
                Item::List(ref l) => {
                    // Serialize a list (sequence)
                    let mut seq = serializer.serialize_seq(Some(l.len()))?;
                    for element in l.iter() {
                        seq.serialize_element(&element)?;
                    }
                    seq.end()
                }

                Item::Program(p) => {
                    let l = coll::List::derive((**p).clone());
                    // Serialize a list (sequence)
                    let mut seq = serializer.serialize_seq(Some(l.len()))?;
                    for element in l.iter() {
                        seq.serialize_element(&element)?;
                    }
                    seq.end()
                }
                Item::Bytes(b) => serializer.serialize_bytes(b.as_slice()),
                Item::Set(s) => {
                    // Serialize a list (sequence)
                    let mut seq = serializer.serialize_seq(Some(s.len()))?;
                    for element in s.iter() {
                        seq.serialize_element(&element)?;
                    }
                    seq.end()
                }
                Item::String(s) => serializer.serialize_str(s.as_str()),

                Item::StaticFile(f) => {
                    executor::block_on(async move { f.read().await.representation() })
                        .serialize(serializer)
                }
                Item::Socket(f) => {
                    executor::block_on(async move { f.read().await.representation() })
                        .serialize(serializer)
                }
                Item::ServerSocket(f) => {
                    executor::block_on(async move { f.read().await.representation() })
                        .serialize(serializer)
                }
                Item::Handoff(h) => h.representation().serialize(serializer),
                Item::Timer(t) => t.representation().serialize(serializer),
                Item::Time => time::representation().serialize(serializer),
                Item::Standard => standard::representation().serialize(serializer),
            }
        }
    }
}
