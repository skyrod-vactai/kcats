//! Support for Associative data types (similar contract to Rust's
//! HashMap). Includes specific runtime data types like Errors,
//! Dictionaries, Environments, as well as generic maps (which are
//! called "associations" in kcats)
use super::{dictionary as dict, environment as env};
use crate::derivation::*;
use crate::traits::*;
use crate::types::container as coll;
use crate::types::container::{Count, Join, Mutey};
use crate::types::number::Int;
use crate::types::*;
use std::collections::HashSet;
use std::convert::Infallible;
use std::sync::{self, Arc};

pub type Associationy<K, V> = HashMap<K, V>;
pub type AssociationContent = Associationy<KeyItem, Item>;
pub type Association = Arc<AssociationContent>;

/// A KeyItem is all the Item types that can be used as a key in an
/// Associative structure. In order to be a key, the type has to be
/// hashable and have an ordering, so types like floating point
/// numbers or sets can't be used.
#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum KeyItem {
    // Order matters here, for comparison purposes - changing the
    // order will change the result of how eg int compares to word.
    Int(Int),
    Char(Char),
    Word(Word),
    Bytes(Bytes),
    String(String),
    List(KeyList),
}

/// An Entry is a single pairing in an Associative type
pub type Entry = (KeyItem, Item);

impl Identity for Entry {}

pub type KeyListContent = coll::Listy<KeyItem>;
pub type KeyList = KeyListContent;

/// An Associative is a container type that associates one Item (the
/// key) with another (the value). It has the property where you can
/// look up a value using the key, and you can update the value that a
/// key points to. Some Item types cannot be used as keys, only
/// [KeyItem] is accepted as an Associative key.
#[derive(Debug, Clone)]
pub enum Associative {
    /// A generic associative structure where you can associate any
    /// [KeyItem] with any [Item].
    Assoc(Association),
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
    Nothing,
}

impl Derive<KeyItem> for Item {
    fn derive(i: KeyItem) -> Self {
        match i {
            KeyItem::Int(i) => Item::Int(i),
            KeyItem::String(i) => i.fit(),
            KeyItem::List(l) => l.iter().cloned().rewrap::<coll::List, Item>().fit(),
            KeyItem::Word(w) => Item::Word(w),
            KeyItem::Bytes(bs) => bs.fit(),
            KeyItem::Char(c) => Item::Char(c),
        }
    }
}

impl TryDerive<&str> for KeyItem {
    fn try_derive(i: &str) -> Result<Self, Error> {
        Word::try_derive(i).map(KeyItem::Word)
    }
}

impl Derive<Word> for KeyItem {
    fn derive(i: Word) -> Self {
        KeyItem::Word(i)
    }
}

impl TryDerive<Item> for KeyItem {
    fn try_derive(i: Item) -> Result<Self, Error> {
        match i {
            Item::Int(i) => Ok(KeyItem::Int(i)),
            Item::Word(w) => Ok(KeyItem::Word(w)),
            Item::Char(c) => Ok(KeyItem::Char(c)),
            i => match coll::Sized::try_derive(i)? {
                coll::Sized::String(i) => Ok(KeyItem::String(i)),

                coll::Sized::Bytes(i) => Ok(KeyItem::Bytes(i)),

                coll::Sized::List(l) => Ok(KeyItem::List((*l).try_rewrap()?)),
                s => {
                    println!("Bad keyitem {:?}", s);
                    Err(Error::expected(fit!("KeyItem"), Item::derive(s)))
                }
            },
        }
    }
}

impl TryDerive<KeyItem> for Word {
    fn try_derive(k: KeyItem) -> Result<Self, Error> {
        match k {
            KeyItem::Word(w) => Ok(w),
            KeyItem::String(s) => s.try_fit(),
            i => Err(Error::expected(fit!("word"), i)),
        }
    }
}

impl PartialEq for Associative {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Associative::Assoc(a), Associative::Assoc(b)) => a == b,
            (Associative::DictEntry(a), Associative::DictEntry(b)) => a == b,
            (Associative::Env(a), Associative::Env(b)) => a == b,
            (Associative::Error(a), Associative::Error(b)) => a == b,
            (Associative::Dictionary(a), Associative::Dictionary(b)) => a == b,
            (Associative::Nothing, Associative::Nothing) => true,
            //(Associative::Assoc(a), b) => Association::derive(a) == Association::derive(b),
            //(a, Associative::Assoc(b)) => Association::derive(a) == Association::derive(b),
            _ => false,
        }
    }
}

impl coll::Join<coll::List> for Association {
    type Output = Association;
    type Error = Error;
    fn join(self, other: coll::List) -> Result<Self::Output, Self::Error> {
        let la = Arc::new(other.try_rewrap()?);
        Ok(self.join(Associative::Assoc(la)).unwrap())
    }
}

impl coll::Join<Association> for coll::List {
    type Output = Association;
    type Error = Error;
    fn join(self, other: Association) -> Result<Self::Output, Self::Error> {
        let la: Association = Arc::new(self.clone().try_rewrap()?);
        Ok(la.join(other).unwrap())
    }
}

impl coll::Join<Association> for Association {
    type Output = Association;
    type Error = Infallible;
    fn join(mut self, other: Association) -> Result<Self::Output, Self::Error> {
        self.mutate()
            .extend(other.iter().map(|(k, v)| (k.clone(), v.clone())));
        Ok(self)
    }
}

// impl coll::Join<Association> for Associative {
//     type Output = Associative;
//     type Error = Infallible;
//     fn join(self, other: Association) -> Result<Self::Output, Error> {
//         let la = Association::try_from_iter(other.iter().cloned())?;
//         self.join(Associative::Assoc(la))
//     }
// }

impl coll::Join<Associative> for Association {
    type Output = Association;
    type Error = Infallible;
    fn join(mut self, other: Associative) -> Result<Self::Output, Self::Error> {
        let thism = self.mutate();
        thism.extend(other);
        Ok(self)
    }
}

/// The join operation is for generic containers, but we can join
/// two Associatives by merging them together. If both
/// Associatives are the same specific type, the type is
/// preserved. If `other` can be converted to the same specific
/// type as `self`, that conversion will be done and the specific
/// type of `self` is preserved. If they are different types and we
/// can't convert `other` to `self`s type, the result will be
/// demoted to a more generic form.
///
/// Keys in `other` have priority over those in `self` - if a key
/// is in both containers, the result will have only the value
/// from `other`.
impl coll::Join<Associative> for Associative {
    type Output = Associative;
    type Error = Infallible;
    fn join(self, other: Associative) -> Result<Self::Output, <Self as Join<Associative>>::Error> {
        //println!("Joining associative to associative");
        Ok(match (self, other) {
            // same type means 2nd one wins.
            //TODO: a little more complex for types that can be extended
            (Associative::DictEntry(_), Associative::DictEntry(other)) => {
                Associative::DictEntry(other)
            }
            (Associative::Dictionary(this), Associative::Dictionary(other)) => {
                Associative::Dictionary(Box::new((*this).join(*other).unwrap()))
            }
            //(Associative::Dictionary(this), Associative::Assoc(other)) => {}
            (Associative::Words(this), Associative::Assoc(other)) => this.join(other).unwrap(),
            (Associative::Assoc(this), Associative::Words(other)) => this.join(other).unwrap(),
            (Associative::Error(_), Associative::Error(other)) => Associative::Error(other),
            (Associative::Env(_), Associative::Env(other)) => Associative::Env(other),
            (Associative::Nothing, Associative::Nothing) => Associative::Nothing,
            // This is infallible so .unwrap should be safe
            (Associative::Assoc(this), other) => Associative::Assoc(this.join(other).unwrap()),
            (this, other) => {
                unimplemented!("Join between associatives: {:?} \n\n{:?}", this, other)
            }
        })
    }
}

impl coll::SimpleTake for Association {
    type Item = Entry;
    fn take_simple(&mut self) -> Option<Self::Item> {
        let maybe_key = self.keys().next().cloned();
        let am = self.mutate();
        let maybe_value = maybe_key.as_ref().and_then(|key| am.remove(key));

        maybe_key.map(|key| (key, maybe_value.unwrap_or_default()))
    }
}

/// The take operation is for generic containers but we can
/// perform it on an Associative by removing an arbitrary pair and
/// returning it.
impl coll::SimpleTake for Associative {
    type Item = Item;
    fn take_simple(&mut self) -> Option<Self::Item> {
        match self {
            Associative::Assoc(ref mut a) => a.take_simple().map(Item::derive),
            Associative::Words(ref mut d) => d.take_simple().map(Item::derive),
            // The remaining impls may require auto-demotion (eg,
            // removing a required field from say, Error). We'll just
            // demote all of them whether the field that is removed is
            // required or not, since the caller cannot know in
            // advance which it will be.
            ref a => {
                let mut assoc: Association = (*a).clone().fit();
                let v = assoc.take_simple().map(Item::derive);
                *self = Associative::Assoc(assoc);
                v
            }
        }
    }
}

impl Associative {
    /// Retuns the number of associations in the container
    pub fn len(&self) -> usize {
        match self {
            Associative::Assoc(a) => a.len(),
            Associative::DictEntry(a) => a.len(),
            Associative::Env(e) => e.len(),
            Associative::Error(e) => e.len(),
            Associative::Words(d) => d.len(),
            Associative::Dictionary(d) => d.len(),
            Associative::Nothing => 0,
        }
    }

    /// Returns true if the container is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Inserts a new association of a [KeyItem] to [Item]. If the key
    /// already exists, the value is replaced and the old value is
    /// returned. If the key doesn't exist, a new one is created with
    /// the new value and no old value is returned. The overall return
    /// value is tuple of an updated Associative, and an optional old
    /// value.
    ///
    /// The Associative returned is not necessarily the same type as
    /// self, as sometimes there is auto-demotion, eg from Error to
    /// Association. Demotion typically happens when you insert a key
    /// into a type that doesn't support that key, you'll get a more
    /// generic type back instead.
    pub fn insert(self, k: KeyItem, v: Item) -> (Associative, Option<Item>) {
        //println!("Insert! {:?}", self);

        match self {
            Associative::Assoc(mut a) => {
                let am = coll::Arc::mutate(&mut a);
                let replaced = am.insert(k, v);
                (Associative::Assoc(a), replaced)
            }
            Associative::Words(mut d) => {
                match (k, v) {
                    (KeyItem::Word(w), e) => {
                        let e2 = e.clone();
                        if let Ok(e) = dict::Entry::try_derive(e) {
                            let dm = coll::Arc::mutate(&mut d);
                            let replaced = dm.insert(w.fit(), e).map(Item::derive);
                            (Associative::Words(d), replaced)
                        } else {
                            // TODO silently failing to insert here is bad
                            println!("Warning, failed to insert into dictionary: {:?}", e2);
                            (Associative::Words(d), None)
                        }
                    }
                    _ => (Associative::Words(d), None),
                }
            }
            Associative::Env(mut e) => {
                let demote = |o: env::Environment, k: assoc::KeyItem, v: Item| {
                    //println!("Demotion!!! {:?}", o);
                    let mut a: assoc::Association = Arc::new(o.rewrap());
                    let am = a.mutate();
                    let old = am.insert(k, v);
                    (assoc::Associative::Assoc(a), old)
                };

                match k {
                    assoc::KeyItem::Word(ref w) => {
                        let s: &str = w.fit();
                        match s {
                            "stack" => {
                                let l = coll::List::try_derive(v.clone());
                                match l {
                                    Ok(l) => {
                                        let old = e.stack.clone();
                                        e.stack = l.fit();
                                        (Associative::Env(e), Some(old.fit()))
                                    }
                                    Err(_) => demote(*e, k, v),
                                }
                            }
                            "program" => {
                                let p = Program::try_derive(v.clone());
                                match p {
                                    Ok(l) => {
                                        let old = e.program.clone();
                                        e.program = l;
                                        (Associative::Env(e), Some(old.fit()))
                                    }
                                    Err(_) => demote(*e, k, v),
                                }
                            }
                            "dictionary" => {
                                let d = dict::Dictionary::try_derive(v.clone());
                                match d {
                                    Ok(d) => {
                                        let old = e.dictionary.clone();
                                        e.dictionary = d;
                                        (Associative::Env(e), Some(old.fit()))
                                    }
                                    Err(_) => demote(*e, k, v),
                                }
                            }

                            _ => demote(*e, k, v),
                        }
                    }
                    k => demote(*e, k, v),
                }
            }
            Associative::DictEntry(mut de) => {
                if let KeyItem::Word(ref w) = k {
                    let w: &str = w.fit();

                    if w == "definition" {
                        let l = coll::List::try_derive(v);
                        if let Ok(l) = l {
                            de.definition = dict::Executable::Derived(std::sync::Arc::new(crate::compile::compile_with_dict(&l, &crate::types::container::dictionary::Dictionary::empty())));
                            // TODO: return the old def
                        }
                    } else if w == "examples" {
                        let l = coll::List::try_derive(v);
                        if let Ok(l) = l {
                            de.examples = Some(l);
                            // TODO: return the old examples
                        }
                    } else if w == "spec" {
                        let l = coll::List::try_derive(v);
                        if let Ok(l) = l {
                            de.spec = l.try_fit().ok();
                            // TODO: return the old spec
                        }
                    }
                }
                (Associative::DictEntry(de), None)
            }
            Associative::Dictionary(mut d) => {
                if let KeyItem::Word(ref w) = k {
                    let w: &str = w.fit();
                    if w == "words" {
                        let e = dict::Words::try_derive(v);
                        if let Ok(words) = e {
                            d.words = words;
                            // TODO: return the old entries
                        }
                    } else if w == "modules" {
                        let l = Vec::<dict::Namespace>::try_derive(v);
                        if let Ok(modules) = l {
                            d.modules = modules;
                            d.resolve();
                            // TODO: return the old modules
                        }
                    }
                };
                (Associative::Dictionary(d), None)
            }
            _ => todo!("insert Implementations for error, dictionary, env etc"),
        }
    }

    /// The put operation is for generic containers, adding a new Item
    /// to the container. In the case of Associative, we can still do
    /// this if the Item is the right type: a key/value pair. If it's
    /// the right type, we [Self::insert] the value using the key,
    /// otherwise return an error.
    pub fn put(self, other: Item) -> Result<Self, Error> {
        match (self, other) {
            (Associative::Words(mut this), other) => {
                let (word, entry) = <(Word, dict::Entry)>::try_derive(other)?;
                let thismut = this.mutate();
                thismut.insert(word.fit(), entry);
                Ok(Associative::Words(this))
            }
            (this, other) => {
                let entry: (KeyItem, Item) = other.try_fit()?;
                let this = this.insert(entry.0, entry.1).0;
                Ok(this)
            }
        }
    }

    /// Retrieves a value from the container using the key
    /// `k`. Returns [None] if the key is not present.
    pub fn get(&self, k: &KeyItem) -> Option<Item> {
        match self {
            Associative::Assoc(a) => a.get(k).cloned(),
            Associative::Dictionary(d) => d.get(k),
            Associative::Error(e) => e.data.get(k).cloned(),
            Associative::Env(e) => e.get(k),
            Associative::DictEntry(d) => d.get(k),
            Associative::Words(d) => match k {
                KeyItem::Word(w) => d.get(w).map(|x| x.clone().fit()),
                _ => None,
            },
            &Associative::Nothing => None,
        }
    }

    /// Returns true if the key `k` is present in the container.
    pub fn contains_key(&self, k: &KeyItem) -> bool {
        match self {
            Associative::Assoc(a) => a.contains_key(k),
            Associative::Error(e) => e.data.contains_key(k),
            Associative::Env(e) => e.contains_key(k),
            Associative::DictEntry(d) => d.contains_key(k),
            Associative::Dictionary(d) => d.contains_key(k),
            Associative::Words(d) => match k {
                KeyItem::Word(w) => d.contains_key(w),
                _ => false,
            },
            &Associative::Nothing => false,
        }
    }

    /// Removes the key `k` from the container, returning a tuple of a
    /// new [Associative] and an optional value if the key was
    /// present.
    pub fn remove(self, k: &KeyItem) -> (Associative, Option<Item>) {
        match self {
            Associative::Assoc(mut a) => {
                let am = coll::Arc::mutate(&mut a);
                let v = am.remove(k);
                (Associative::Assoc(a), v)
            }
            Associative::Words(mut d) => {
                let dm = coll::Arc::mutate(&mut d);
                let v = dm.remove(&Word::try_derive(k.clone()).unwrap_or_default());
                (Associative::Words(d), v.map(|v| v.fit()))
            }
            Associative::Error(mut e) => {
                let a = e.data.mutate();
                let v = a.remove(k);
                (Associative::Error(e), v)
            }
            Associative::Env(e) => {
                let a: Association = Arc::new((*e).rewrap());
                Associative::Assoc(a).remove(k)
            }
            _ => todo!("Removing from other associative types"),
        }
    }
}

// impl ToIterator for Association {
//     type Item = Entry;
//     type IntoIter = Box<dyn Iterator<Item = Entry>>;

//     fn to_iter<'a>(self) -> Self::IntoIter {
//         let items: Vec<_> = self.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
//         Box::new(items.into_iter())
//     }
// }

impl IntoIterator for Associative {
    type Item = Entry;
    type IntoIter = Box<dyn Iterator<Item = Entry>>;

    fn into_iter<'a>(self) -> Self::IntoIter {
        match self {
            Associative::Assoc(a) => Box::new(a.as_ref().clone().into_iter()),
            Associative::DictEntry(e) => Box::new(e.into_iter()),
            Associative::Dictionary(d) => Box::new(d.into_iter()),
            Associative::Words(d) => Box::new(d.as_ref().clone().into_iter().map(|(k, v)| {
                (
                    crate::types::container::associative::KeyItem::Word(k),
                    crate::types::Item::DictEntry(Box::new(v)),
                )
            })),
            Associative::Error(e) => Box::new(e.into_iter()),
            Associative::Env(e) => Box::new(e.as_ref().clone().into_iter()),
            Associative::Nothing => Box::new(std::iter::empty()),
        }
    }
}

impl TryDerive<Item> for Associative {
    fn try_derive(i: Item) -> Result<Associative, Error> {
        match i {
            Item::Assoc(a) => Ok(Associative::Assoc(a)),
            Item::DictEntry(a) => Ok(Associative::DictEntry(a)),
            Item::Env(a) => Ok(Associative::Env(a)),
            Item::Error(a) => Ok(Associative::Error(a)),
            Item::Words(a) => Ok(Associative::Words(a)),
            Item::Dictionary(a) => Ok(Associative::Dictionary(a)),
            //Item::List(l) => Association::derive_iter(l.iter().cloned()),
            Item::List(l) => (*l)
                .try_rewrap::<AssociationContent>()
                .map(Arc::new)
                .map(Associative::Assoc),
            i => Err(Error::expected(fit!("associative"), i)),
        }
    }
}

impl TryDerive<coll::Sized> for Associative {
    fn try_derive(i: coll::Sized) -> Result<Associative, Error> {
        match i {
            coll::Sized::Assoc(a) => Ok(Associative::Assoc(a)),
            coll::Sized::DictEntry(a) => Ok(Associative::DictEntry(a)),
            coll::Sized::Env(a) => Ok(Associative::Env(a)),
            coll::Sized::Error(a) => Ok(Associative::Error(a)),
            coll::Sized::Words(a) => Ok(Associative::Words(a)),
            coll::Sized::Dictionary(a) => Ok(Associative::Dictionary(a)),
            coll::Sized::List(l) => {
                Associative::try_derive(*l)

                // <Result<Associative, NestedDeriveError<coll::List>>>::derive(*l)
                //     .map_err(|e| e.update(|s| coll::Sized::List(Box::new(s))))
            }
            i => Err(Error::expected(fit!("associative"), i)),
        }
    }
}

impl TryDerive<coll::List> for Associative {
    fn try_derive(l: coll::List) -> Result<Associative, Error> {
        l.try_rewrap::<AssociationContent>()
            .map(Arc::new)
            .map(Associative::Assoc)
    }
}

// Convert anything that can be iterated over as Items, to an
// Association. The items must be pairs that are
// convertable to Entry, otherwise it will return an error.

impl Derive<HashMap<KeyItem, Item>> for Association {
    fn derive(h: HashMap<KeyItem, Item>) -> Self {
        sync::Arc::new(h)
    }
}

impl Derive<Entry> for Item {
    fn derive(e: Entry) -> Item {
        list![e.0, e.1].fit()
    }
}

impl TryDerive<Item> for Entry {
    fn try_derive(i: Item) -> Result<Entry, Error> {
        let s = coll::Sized::try_derive(i)?;
        if s.count() != 2 {
            Err(Error::expected(fit!("pair"), s))
        } else {
            let mut iter = s.into_iter();
            let key: KeyItem = iter.next().unwrap().try_fit()?;
            let value = iter.next().unwrap();
            Ok((key, value))
        }
    }
}

impl Derive<Associative> for Association {
    fn derive(a: Associative) -> Association {
        match a {
            Associative::Assoc(a) => a,
            a => Arc::new(a.clone().into_iter().rewrap()),
        }
    }
}

impl Derive<AssociationContent> for Item {
    fn derive(a: AssociationContent) -> Item {
        sync::Arc::new(a).fit()
    }
}

impl Derive<Association> for Item {
    fn derive(a: Association) -> Item {
        Associative::Assoc(a).fit()
    }
}

impl Derive<Associative> for Item {
    fn derive(a: Associative) -> Item {
        match a {
            Associative::Assoc(a) => Item::Assoc(a),
            Associative::DictEntry(a) => Item::DictEntry(a),
            Associative::Dictionary(a) => Item::Dictionary(a),
            Associative::Env(a) => Item::Env(a),
            Associative::Error(a) => Item::Error(a),
            Associative::Words(a) => Item::Words(a),
            Associative::Nothing => Item::default(),
        }
    }
}

impl Derive<Associative> for coll::Sized {
    fn derive(a: Associative) -> coll::Sized {
        match a {
            Associative::Assoc(a) => coll::Sized::Assoc(a),
            Associative::DictEntry(a) => coll::Sized::DictEntry(a),
            Associative::Dictionary(a) => coll::Sized::Dictionary(a),
            Associative::Env(a) => coll::Sized::Env(a),
            Associative::Error(a) => coll::Sized::Error(a),
            Associative::Words(a) => coll::Sized::Words(a),
            Associative::Nothing => Default::default(),
        }
    }
}

impl Derive<(KeyItem, Item)> for KeyItem {
    fn derive((k, _): (KeyItem, Item)) -> KeyItem {
        k
    }
}

/// Converting Associative to Set just returns the keys.
impl Derive<Associative> for coll::Set {
    fn derive(a: Associative) -> coll::Set {
        Arc::new(HashSet::from_iter(a.clone().into_iter().map(|(k, _)| k)))
    }
}

pub trait Convert<KA, VA> {
    /// Convert from any type of hashmap to any other, assuming the keys
    /// and values convert
    fn convert<KB, VB>(&self) -> Result<HashMap<KB, VB>, Error>
    where
        KB: Clone + Eq + Hash + TryDerive<KA>,
        VB: Clone + TryDerive<VA>,
        KA: Clone + Eq + Hash, // Assuming Clone is needed for TryFrom
        VA: Clone;
}

impl<KA, VA> Convert<KA, VA> for HashMap<KA, VA>
where
    KA: Eq + Hash + Clone,
    VA: Clone,
{
    fn convert<KB, VB>(&self) -> Result<HashMap<KB, VB>, Error>
    where
        KB: Clone + Eq + Hash + TryDerive<KA>,
        VB: Clone + TryDerive<VA>,
        KA: Clone + Eq + Hash, // Assuming Clone is needed for TryFrom
        VA: Clone,
    {
        let mut new_hashmap = HashMap::new();

        for (key, value) in self.iter().map(|(k, v)| (k.clone(), v.clone())) {
            let new_key: KB = key.try_fit()?;
            let new_value: VB = value.try_fit()?;
            new_hashmap.insert(new_key, new_value);
        }

        Ok(new_hashmap)
    }
}

// these types are also not errors so mark them as such
impl OkItem for Association {}
impl OkItem for Associative {}
impl OkItem for AssociationContent {}
impl OkItem for KeyItem {}

mod serde {
    use super::KeyItem;

    use serde::de::{self, Deserialize, Deserializer, Visitor};
    use serde::ser::{Serialize, SerializeSeq};
    use std::fmt;

    struct KeyItemVisitor;

    impl<'de> Visitor<'de> for KeyItemVisitor {
        type Value = KeyItem;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("expected a specific representation for Item")
        }

        fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(KeyItem::Int(value))
        }

        fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(KeyItem::Int(value as i64))
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(KeyItem::String(v.to_string()))
        }

        fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(KeyItem::Bytes(v))
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: de::SeqAccess<'de>,
        {
            let mut items: Vec<KeyItem> = Vec::new();
            while let Some(item) = seq.next_element::<KeyItem>()? {
                items.push(item);
            }
            Ok(KeyItem::List(items.into()))
        }
    }

    impl<'de> Deserialize<'de> for KeyItem {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_any(KeyItemVisitor)
        }
    }

    impl Serialize for KeyItem {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            match self {
                KeyItem::Int(i) => serializer.serialize_i64(*i),
                KeyItem::Word(w) => serializer.serialize_str(w.data.as_str()),
                KeyItem::Char(c) => serializer.serialize_char(*c),
                KeyItem::Bytes(b) => serializer.serialize_bytes(b.as_slice()),
                KeyItem::List(ref l) => {
                    // Serialize a list (sequence)
                    let mut seq = serializer.serialize_seq(Some(l.len()))?;
                    for element in l.iter() {
                        seq.serialize_element(&element)?;
                    }
                    seq.end()
                }
                KeyItem::String(s) => serializer.serialize_str(s.as_str()),
            }
        }
    }
}
