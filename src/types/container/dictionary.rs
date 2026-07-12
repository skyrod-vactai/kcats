use super::associative as assoc;
use crate::axiom;
use crate::axiom::BUILTIN_FUNCTIONS;
use crate::derivation::*;
use crate::serialize;
use crate::stdlib_hashes;
use crate::traits::*;
use crate::types::container::associative::Convert;
use crate::types::container::{self as coll, Count, Mutey};
use crate::types::{self, Bytes, Error, Item, Word, WordData};
use crate::{fit, list};
use core::fmt;
use internment::Intern;
use std::collections::HashMap;
use std::collections::HashSet;
use std::convert::Infallible;
use std::hash::Hash;
use std::mem;
use std::ptr;
use std::sync::Arc;
use std::sync::OnceLock;

/// The definition of a [Word], contains its actual code (the
/// definition), and also documentation like specs and examples.
#[derive(Debug, Clone, PartialEq)]
pub struct Entry {
    pub examples: Option<coll::List>,
    pub spec: Option<Spec>,
    pub definition: Executable,
    pub namespace: Namespace,
    pub doc: Option<String>,
}

impl Eq for Entry {}
// TODO: move specs to their own module
/// An element of a [Spec], either an input or an output. Holds the
/// type and optional name of the input/output.
#[derive(Debug, Clone, PartialEq)]
pub struct SpecElement {
    pub elemtype: types::Word,
    pub name: Option<types::Word>,
}

pub type StackSpec = Vec<SpecElement>;

/// The spec of a [Word] consists of the input spec and the output
/// spec, that shows what the stack should look like before and after
/// the [Word] is invoked.
pub type Spec = (StackSpec, StackSpec);

impl TryDerive<Item> for SpecElement {
    fn try_derive(i: Item) -> Result<SpecElement, Error> {
        match i {
            Item::Word(w) => Ok(SpecElement {
                elemtype: w,
                name: None,
            }),
            i => {
                let s = coll::List::try_derive(i)?;
                if s.len() != 2 {
                    Err(Error::list_count(2))
                } else {
                    let t = types::Word::try_derive(s.front().unwrap().clone())?;
                    let n = types::Word::try_derive(s.get(1).unwrap().clone())?;
                    Ok(SpecElement {
                        elemtype: t,
                        name: Some(n),
                    })
                }
            }
        }
    }
}

impl TryDerive<coll::List> for StackSpec {
    fn try_derive(s: coll::List) -> Result<StackSpec, Error> {
        s.iter()
            .cloned()
            .map(SpecElement::try_derive)
            //.map(|r| r.and_then(SpecElement::try_derive))
            .collect::<Result<StackSpec, Error>>()
    }
}

impl TryDerive<coll::List> for Spec {
    fn try_derive(s: coll::List) -> Result<Spec, Error> {
        if s.len() != 2 {
            Err(Error::list_count(2))
        } else {
            Ok((
                StackSpec::try_derive(coll::List::try_derive(s.front().unwrap().clone())?)?,
                StackSpec::try_derive(coll::List::try_derive(s.get(1).unwrap().clone())?)?,
            ))
        }
    }
}

impl TryDerive<Item> for Spec {
    fn try_derive(i: Item) -> Result<Spec, Error> {
        Spec::try_derive(coll::List::try_derive(i)?)
    }
}

impl Derive<SpecElement> for Item {
    fn derive(se: SpecElement) -> Item {
        if se.name.is_some() {
            crate::types::Item::List(Box::new(
                vec![se.elemtype.fit(), se.name.unwrap().fit()].into(),
            ))
        } else {
            Item::Word(se.elemtype)
        }
    }
}

impl Derive<Spec> for Item {
    fn derive(s: Spec) -> Item {
        let l1 = crate::types::Item::List(Box::new(s.0.into_iter().map(|e| e.fit()).collect()));
        let l2 = crate::types::Item::List(Box::new(s.1.into_iter().map(|e| e.fit()).collect()));
        crate::types::Item::List(Box::new(vec![l1, l2].into()))
    }
}

impl Derive<Namespace> for Item {
    fn derive(ns: Namespace) -> Item {
        (*ns).clone().fit()
    }
}

impl IntoList for Vec<Namespace> {}
impl IntoList for Vec<SpecElement> {}

//impl Derive<Intern<Vec<u8>>> for

impl Entry {
    pub fn len(&self) -> usize {
        3 // 3 fields
    }

    pub fn is_empty(&self) -> bool {
        false
    }

    pub fn get(&self, key: &assoc::KeyItem) -> Option<Item> {
        match key {
            assoc::KeyItem::Word(w) => match w.data.as_str() {
                "spec" => self.spec.clone().map(|x| x.fit()),
                "examples" => self.examples.clone().map(|x| x.fit()),
                "definition" => Some(match self.definition.clone() {
                    Executable::Axiom(a) => a.clone().fit(),
                    Executable::Derived(d) => {
                        crate::compile::decompile(&d).fit()
                    },
                }),
                "namespace" => Some(self.namespace.fit()),
                "doc" => self.doc.clone().map(|x| x.fit()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn contains_key(&self, key: &assoc::KeyItem) -> bool {
        types::Word::try_derive(key.clone())
            .is_ok_and(|ref w| matches!(w.fit(), "examples" | "spec" | "definition" | "doc"))
    }
}

// TODO: Use the builtin Bytes type
pub type Namespace = Intern<Vec<u8>>;

pub fn bytes_to_ns(b: Bytes) -> Namespace {
    if b.is_empty() {
        Default::default()
    } else {
        Intern::new(b)
    }
}

impl TryDerive<Item> for Namespace {
    fn try_derive(i: Item) -> Result<Namespace, Error> {
        match i {
            Item::Bytes(b) => Ok(bytes_to_ns(*b)),
            i => {
                let s = coll::Sized::try_derive(i)?;
                if s.is_empty() {
                    Ok(Default::default())
                } else {
                    Err(Error::expected(fit!("namespace"), s))
                }
            }
        }
    }
}

/// Holds [Word]s and their definitions.
pub type Words = coll::Arc<HashMap<Word, Entry>>;

/// One of the main components of an
/// [crate::types::container::environment::Environment]. Provides
/// definitions of words and list of modules, which decides which
/// definition of the same word to use (based on which module it comes
/// from). The lingo field is a cache of which words are selected.
#[derive(Clone, PartialEq)]
#[derive(Default)]
pub struct Dictionary {

    pub words: Words,
    pub lingo: Words,
    pub modules: Vec<Namespace>,
}

/// A namespace for the core functions
pub static CORE: OnceLock<Namespace> = OnceLock::new();
pub static CORE_NAMESPACES: OnceLock<HashMap<&'static str, Namespace>> = OnceLock::new();

/// A custom impl for Dictionary that doesn't dump a massive data
/// structure. comment this out to get access to the full debug output.
impl fmt::Debug for Dictionary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Dictionary")
            .field("words", &format_args!("Words(len={})", self.words.len()))
            .field("modules", &self.modules)
            .finish()
    }
}

impl Dictionary {
    pub fn empty() -> &'static Self {
        static EMPTY: std::sync::OnceLock<Dictionary> = std::sync::OnceLock::new();
        EMPTY.get_or_init(|| Dictionary::default())
    }

    /// Treats the [Dictionary] as an associative structure,
    /// returning one of its fields, or [None].
    pub fn get(&self, key: &assoc::KeyItem) -> Option<Item> {
        match key {
            assoc::KeyItem::Word(w) => match w.data.as_str() {
                "words" => Some(self.words.clone().fit()),
                "modules" => Some(crate::types::Item::List(Box::new(
                    self.modules
                        .iter()
                        .map(|m| crate::types::Item::Bytes(Box::new(m.as_ref().clone())))
                        .collect(),
                ))),
                "lingo" => Some(self.lingo.clone().fit()),
                _ => None,
            },
            _ => None,
        }
    }

    /// Get an [Entry] from the dictionary, doing namespace
    /// resolution.
    pub fn get_entry(&self, key: &types::Word) -> Option<Entry> {
        self.lingo.get(key).cloned()
    }

    pub fn len(&self) -> usize {
        2
    }

    pub fn is_empty(&self) -> bool {
        false
    }

    pub fn merge(&mut self, new: Self, namespace: &Namespace) {
        self.words.merge(new.words, namespace);
        self.resolve();
    }

    pub fn contains_key(&self, key: &assoc::KeyItem) -> bool {
        types::Word::try_derive(key.clone())
            .is_ok_and(|ref w| matches!(w.fit(), "modules" | "words" | "lingo"))
    }

    /// Produce an [Words] map that is pre-resolved using the
    /// modules from this dictionary. Saves computation at runtime
    /// because resolution is already done.
    pub fn resolve(&mut self) {
        // Re-compilation optimization pass:
        // During bootstrap (or dynamic parsing), code is initially compiled against
        // `Dictionary::default()`. This guarantees the code is 100% valid and runnable,
        // preventing uncompiled execution bugs, but misses out on inlining custom
        // shuffles because the dictionary was incomplete at parsing time.
        // Because `resolve()` is called after loading core words as an atomic unit,
        // we can re-compile all derived words using the now fully-loaded dictionary
        // to cleanly apply all inlining optimizations, regardless of definition order.
        let mut to_recompile = Vec::new();
        for (w, entry) in self.words.iter() {
            if let Executable::Derived(chunk) = &entry.definition {
                if let Some(source) = &chunk.source {
                    to_recompile.push((w.clone(), source.clone()));
                }
            }
        }
        let mut recompiled = Vec::new();
        for (w, source) in to_recompile {
            let chunk = crate::compile::compile_with_dict(&source, self);
            recompiled.push((w, Executable::Derived(std::sync::Arc::new(chunk))));
        }
        let words = self.words.mutate();
        for (w, def) in recompiled {
            if let Some(entry) = words.get_mut(&w) {
                entry.definition = def;
            }
        }

        fn group_by_namespace(words: &Words) -> HashMap<Namespace, Vec<(types::Word, Entry)>> {

            let mut grouped: HashMap<Namespace, Vec<(types::Word, Entry)>> = HashMap::new();

            for (k, v) in words.iter() {
                grouped
                    .entry(k.namespace.unwrap_or_default())
                    .or_default()
                    .push((
                        {
                            // Block out the namespace, since we want to
                            // find the word using no namespace
                            let mut kk = k.as_ref().clone();
                            kk.namespace = None;
                            kk.fit()
                        },
                        v.clone(),
                    ));
            }
            grouped
        }
        let by_ns = group_by_namespace(&self.words);
        //println!("by namespace: {:?}", by_ns);
        let mut cache = Words::fresh();
        let mcache = cache.mutate();

        for module in self.modules.iter() {
            //println!("Adding {:?} to cache", module);
            mcache.extend(by_ns.get(module).cloned().unwrap_or_default())
        }

        // println!(
        //     "Cache now selected {} words. modules: {:?}",
        //     cache.len(),
        //     by_ns.keys()
        // );
        // println!(
        //     "resolve: contains? {:?}",
        //     cache.get(Word::try_derive("contains?").unwrap().as_ref())
        // );
        //println!("Example key {:?}", cache.keys().find(|_| true));
        self.lingo = cache;
        //println!("After resolve: {:?}", self);
    }

    pub fn using(&mut self, mut namespaces: Vec<Namespace>) {
        // Swap so that we're putting the new namespaces as higher priority
        // by making them first in the vec
        mem::swap(&mut self.modules, &mut namespaces);
        self.modules.extend(namespaces);
        self.resolve();
    }

    pub fn loaded_namespaces(&self) -> HashSet<Namespace> {
        self.words.iter().map(|(_, w)| w.namespace).collect()
    }
}

pub trait Dict {
    /// Returns the difference between this dictionary and a "newer"
    /// one: The additions/updates, and the deletions.
    fn diff(&self, newer: &Words) -> (Vec<(Word, Entry)>, Vec<Word>);

    /// Takes a core module (in string form - should contain a series
    /// of word definitions, not wrapped in a single list), and
    /// inserts all the definitions into the dictionary, with an
    /// optional namespace.
    fn insert_core_module(&mut self, lexicon: String) -> Result<Namespace, Error>;

    /// For stdlib words that are both built-in and part of a module
    /// that isn't necessarily loaded as part of the standard
    /// environment, we need to be able to link the word to its rust
    /// definition. Leaves other fields as None to be filled in later.
    fn builtins() -> Self;

    /// Merges this dictionary with the given new dictionary. The new
    /// words are added with the given namespace.
    fn merge(&mut self, new: Words, namespace: &Namespace);
}

impl coll::SimpleTake for Words {
    type Item = (Word, Entry);
    fn take_simple(&mut self) -> Option<Self::Item> {
        if let Some(ref k) = self.keys().next().cloned() {
            let dm = self.mutate();
            let v = dm.remove(k).unwrap();
            Some((*k, v))
        } else {
            None
        }
    }
}

use sha2::{self, Digest};

fn hash(s: String) -> Vec<u8> {
    sha2::Sha256::digest(s.as_bytes()).to_vec()
}

/// Instead of blindly extending dictionaries, we don't overwrite
/// entries that are axiom word defs. The idea is that when there is
/// an existing key that is an axiom word, and then we get a new value
/// that's also an axiom (no definition) - we don't overwrite the
/// existing definition
fn merge_entries(orig: &mut Entry, new: Entry) {
    orig.examples = new.examples;
    orig.spec = new.spec;
    orig.doc = new.doc;
    // Don't overwrite namespace, should be the same anyway.  Don't
    // blindly overwrite the definition, this could be an axiom word
    // where we've left the definition out, and we're merging with the
    // builtin, now that we've read the lexicon. The definition is the
    // builtin and we want to keep that.
    match (orig.definition.clone(), new.definition) {
        (Executable::Axiom(_), Executable::Derived(d)) if d.ops.is_empty() || (d.ops.len() == 1 && matches!(d.ops[0], crate::types::container::program::Op::Return)) => {
            // println!("merge_entries: kept axiom {}", a.name);
        } // keep axiom, this is a dummy from .kcats
        (Executable::Axiom(_a), Executable::Derived(d)) => {
            // println!("merge_entries: overwrote axiom {} with derived", a.name);
            orig.definition = Executable::Derived(d); // user override
        }
        (Executable::Axiom(_), Executable::Axiom(_)) => {} // don't overwrite axiom with axiom
        (_, Executable::Derived(d)) => {
            orig.definition = Executable::Derived(d); // both derived? overwrite
        }
        (Executable::Derived(_), _) => {} // don't overwrite a derived with axiom, security issue
    }
}

/// Merge hashmaps with an entry merging function, similar to
/// "merge-with" in some other languages.
fn merge_smart<K, V, F>(base: &mut HashMap<K, V>, incoming: HashMap<K, V>, merger: F)
where
    K: Eq + Hash,
    F: Fn(&mut V, V), // Takes mutable reference to old, owned new
{
    for (key, new_val) in incoming {
        match base.entry(key) {
            // Case 1: Key exists. We have mutable access to old value.
            // We pass ownership of `new_val` to the merger.
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                merger(entry.get_mut(), new_val);
            }
            // Case 2: Key does not exist.
            // We insert `new_val` directly.
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(new_val);
            }
        }
    }
}

impl Dict for Words {
    fn diff(&self, newer: &Words) -> (Vec<(Word, Entry)>, Vec<Word>) {
        diff_hashmaps(self, newer)
    }

    fn merge(&mut self, new: Words, namespace: &Namespace) {
        let (adds, _) = self.diff(&new);
        // add namepaces to the adds and deletes
        //println!("Merge {} adds, {} deletes", adds.len(), deletes.len());

        let adds: HashMap<Word, Entry> = adds
            .into_iter()
            .map(|(w, mut e)| {
                let mut w = w.as_ref().clone();
                w.namespace = Some(*namespace);
                e.namespace = *namespace;
                //println!("Adding {:?}", w);
                (Intern::new(w), e)
            })
            .collect();
        let sm = self.mutate();
        merge_smart(sm, adds, merge_entries);
    }

    /// Load core modules. For now, we cheat and assume it's in the
    /// format of `[list-of-definitions] join`, and we don't actually
    /// evaluate the join like we will do later in bootstrapping.
    fn insert_core_module(&mut self, lexicon: String) -> Result<Namespace, Error> {
        //println!("Parsing: {}", lexicon);
        let dict = self.mutate();

        let hash = hash(lexicon.clone());
        let namespace = Intern::new(hash);
        let items = serialize::parse(lexicon)?
            .pop_front()
            .ok_or_else(|| Error::short_list(1))
            .and_then(coll::List::try_derive)?;
        for r in Box::new(items.iter().cloned()) {
            let (k, def): (assoc::KeyItem, Item) = r.try_fit().unwrap();
            let mut word: WordData = Word::try_derive(k).unwrap().as_ref().clone();
            word.namespace = Some(namespace);
            let iter: Box<dyn Iterator<Item = Item>> = def.try_fit().unwrap();
            let new_entry: Entry = iter.try_fit().unwrap();
            let new_entry2 = new_entry.clone();

            dict.entry(word.fit())
                .and_modify(|e| {
                    e.examples = new_entry.examples;
                    e.spec = new_entry.spec;
                    e.doc = new_entry.doc;
                    e.namespace = namespace;
                    // Don't overwrite the definition, this should be
                    // an axiom word where we've left the
                    // spec/examples temporarily blank and we're
                    // filling them in now that we've read the
                    // lexicon. The definition is the builtin and we
                    // want to keep that.
                    e.definition = match (e.definition.clone(), new_entry.definition) {
                        (Executable::Axiom(a), _) => Executable::Axiom(a),
                        (_, Executable::Derived(d)) => Executable::Derived(d),
                        (Executable::Derived(d), _) => Executable::Derived(d), //(None, None) => None
                    }
                })
                .or_insert(new_entry2);
        }

        Ok(namespace)
    }

    fn builtins() -> Self {
        // base64 encodes to "core"
        let core_nss = CORE_NAMESPACES.get_or_init(|| {
            stdlib_hashes::get_manifest()
                .into_iter()
                .map(|(k, v)| {
                    // Convert &'static [u8] -> Vec<u8> -> Intern<Vec<u8>>
                    (k, Intern::new(v.to_vec()))
                })
                .collect()
        });
        let mut dict = HashMap::new();
        for (bw, bd) in BUILTIN_FUNCTIONS.iter() {
            let ns = core_nss
                .get(bw.data.as_str())
                .unwrap_or_else(|| panic!("Missing namespace for builtin: '{}', core_nss len: {}, contains 'and': {}", bw.data.as_str(), core_nss.len(), core_nss.contains_key("and")));
            let entry = Entry {
                definition: bd.clone(),
                examples: None,
                spec: None,

                namespace: *ns,
                doc: None,
            };
            let mut bw = bw.as_ref().clone();
            bw.namespace = Some(*ns);
            dict.insert(bw.fit(), entry);
        }
        Arc::new(dict)
    }
}

/// Returns an owned pair given a pair of references
fn owned<T: Clone, U: Clone>(entry: (&T, &U)) -> (T, U) {
    (entry.0.clone(), entry.1.clone())
}

/// Returns the differences between two hashmaps, including the keys
/// that have been added or changed (including the new values), and
/// the keys that were deleted.
fn diff_hashmaps<K, V>(a: &HashMap<K, V>, b: &HashMap<K, V>) -> (Vec<(K, V)>, Vec<K>)
where
    K: Eq + Hash + Clone,
    V: PartialEq + Clone,
{
    let a_keys: HashSet<K> = a.keys().cloned().collect();
    let b_keys: HashSet<K> = b.keys().cloned().collect();

    // Keys that are in `b` but not in `a` or have updated values in `b`
    let added_or_updated: Vec<(K, V)> = b
        .iter()
        .filter(|(k, v)| !a_keys.contains(k) || a.get(k) != Some(v))
        .map(owned)
        .collect();

    // Keys that are in `a` but not in `b`
    let deleted: Vec<K> = a_keys.difference(&b_keys).cloned().collect();

    (added_or_updated, deleted)
}

impl coll::Join<Words> for Words {
    type Output = Words;
    type Error = Infallible;
    fn join(mut self, other: Words) -> Result<Self::Output, Self::Error> {
        let sm = self.mutate();
        sm.extend(other.iter().map(owned));
        Ok(self)
    }
}

impl coll::Join<Dictionary> for Dictionary {
    type Output = Dictionary;
    type Error = Infallible;
    fn join(mut self, other: Self) -> Result<Self::Output, Self::Error> {
        self.words = self.words.join(other.words)?;
        self.modules.extend(other.modules);
        self.resolve();
        Ok(self)
    }
}

/// When joining two identically shaped map-like structs, it's just last one wins.
impl coll::Join<Entry> for Entry {
    type Output = Entry;
    type Error = Infallible;
    fn join(self, other: Self) -> Result<Self::Output, Self::Error> {
        Ok(other)
    }
}

impl coll::Join<assoc::Association> for Words {
    type Output = assoc::Associative;
    type Error = Infallible;
    fn join(mut self, other: assoc::Association) -> Result<Self::Output, Self::Error> {
        // Try to convert to dictionary type
        //println!("dict + assoc join");
        match other.convert::<Word, Entry>() {
            Ok(d) => {
                let tm = self.mutate();
                tm.extend(d);
                Ok(assoc::Associative::Words(self))
            }
            // TODO: convert the other way (to assoc) instead
            Err(_) => {
                //println!("Conversion error: {:?}", e);
                Ok(assoc::Associative::Words(self))
            }
        }
    }
}

impl coll::Join<Words> for assoc::Association {
    type Output = assoc::Associative;
    type Error = Infallible;
    fn join(self, mut other: Words) -> Result<Self::Output, Self::Error> {
        // Try to convert to dictionary type
        //println!("assoc + dict join");
        Ok(match self.convert::<Word, Entry>() {
            Ok(d) => {
                let tm = other.mutate();
                tm.extend(d.iter().map(owned));
                assoc::Associative::Words(other)
            }
            // TODO: convert the other way (to assoc) instead
            Err(_) => assoc::Associative::Words(other),
        })
    }
}

/// The actual code for what a [Word] should do.
#[derive(Clone)]
pub enum Executable {
    /// A definition in the base language - a rust function that
    /// modifies the environment.
    Axiom(&'static axiom::Builtin),
    /// A definition in terms of other [Word]s - a kcats program
    Derived(Arc<crate::types::container::program::Chunk>),
}

// dictionary words are equal if they have the same function reference,
// no need to compare the function values
impl PartialEq for Executable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Executable::Axiom(s), Executable::Axiom(o)) => ptr::eq(*s, *o),
            (Executable::Derived(s), Executable::Derived(o)) => s == o,
            _ => false,
        }
    }
}

impl fmt::Debug for Executable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Executable::Axiom(a) => f.write_str(format!("builtin_{}", a.name).as_str()),
            Executable::Derived(d) => {
                let mut ds = f.debug_list();
                let list = crate::compile::decompile(&d);
                ds.entries(list.iter());
                ds.finish()
            }
        }
    }
}

impl IntoIterator for Entry {
    type Item = assoc::Entry;
    type IntoIter = Box<dyn Iterator<Item = assoc::Entry>>;

    fn into_iter(self) -> Self::IntoIter {
        let mut v: Vec<(assoc::KeyItem, Item)> = vec![(fit!("definition"), {
            match self.definition {
                Executable::Derived(l) => {
                    crate::compile::decompile(&l).fit()
                },
                Executable::Axiom(a) => a.clone().fit(),
            }
        })];
        if let Some(e) = self.examples {
            v.push((fit!("examples"), e.fit()));
        }
        if let Some(s) = self.spec {
            v.push((fit!("spec"), s.fit()))
        }
        if let Some(d) = self.doc {
            v.push((fit!("doc"), d.fit()))
        }
        v.push((fit!("namespace"), self.namespace.as_ref().clone().fit()));
        Box::new(v.into_iter())
    }
}

impl IntoIterator for Dictionary {
    type Item = assoc::Entry;
    type IntoIter = Box<dyn Iterator<Item = assoc::Entry>>;

    fn into_iter(self) -> Self::IntoIter {
        let v: Vec<(assoc::KeyItem, Item)> = vec![
            (fit!("words"), self.words.fit()),
            (
                fit!("modules"),
                crate::types::Item::List(Box::new(
                    self.modules
                        .iter()
                        .map(|m| crate::types::Item::Bytes(Box::new(m.as_ref().clone())))
                        .collect(),
                )),
            ),
            (fit!("lingo"), self.lingo.fit()),
        ];
        Box::new(v.into_iter())
    }
}

impl TryDerive<Box<dyn Iterator<Item = Item>>> for Entry {
    fn try_derive(iter: Box<dyn Iterator<Item = Item>>) -> Result<Self, Error> {
        let mut examples: Option<coll::List> = None;
        let mut definition: Option<Executable> = None;
        let mut spec: Option<Spec> = None;
        let mut namespace: Namespace = Default::default();
        let mut doc: Option<String> = None;
        for i in iter {
            let (k, v): (assoc::KeyItem, Item) = i.try_fit()?;
            //println!("k: {:?}, v: {:?}", k, v);
            if k == fit!("examples") {
                examples = Some(v.try_fit()?);
            } else if k == fit!("definition") {
                definition = Some(v.try_fit()?);
            } else if k == fit!("spec") {
                spec = v.try_fit().ok();
            } else if k == fit!("namespace") {
                namespace = v.try_fit().unwrap_or_default();
            } else if k == fit!("doc") {
                doc = v.try_fit().ok();
            } else {
                continue;
            }
        }
        Ok(Entry {
            examples,
            definition: definition.unwrap_or_else(|| Executable::Derived(Arc::new(crate::compile::compile_with_dict(&coll::List::default(), &crate::types::container::dictionary::Dictionary::default())))),
            spec,
            namespace,
            doc,
        })
    }
}

impl TryDerive<Box<dyn Iterator<Item = Item>>> for Words {
    fn try_derive(iter: Box<dyn Iterator<Item = Item>>) -> Result<Self, Error> {
        iter.map(<(Word, Entry)>::try_derive)
            .collect::<Result<HashMap<Word, Entry>, Error>>()
            .map(Arc::new)
    }
}

impl TryDerive<Box<dyn Iterator<Item = Item>>> for Dictionary {
    fn try_derive(iter: Box<dyn Iterator<Item = Item>>) -> Result<Self, Error> {
        let mut words = Words::default();
        let mut modules = Vec::<Namespace>::default();

        for i in iter {
            let (k, v): (assoc::KeyItem, Item) = i.try_fit()?;
            //println!("k: {:?}, v: {:?}", k, v);
            if k == fit!("words") {
                words = v.try_fit()?;
            } else if k == fit!("modules") {
                modules = v.try_fit()?;
            } else {
                continue;
            }
        }
        let mut dict = Dictionary {
            words,
            modules,
            lingo: Default::default(),
        };
        dict.resolve();
        Ok(dict)
    }
}

impl TryDerive<Item> for Executable {
    fn try_derive(i: Item) -> Result<Self, Error> {
        coll::List::try_derive(i).map(|l| Executable::Derived(Arc::new(crate::compile::compile_with_dict(&l, &crate::types::container::dictionary::Dictionary::default()))))
    }
}

impl TryDerive<Item> for Entry {
    fn try_derive(i: Item) -> Result<Self, Error> {
        let s = coll::Sized::try_derive(i)?;
        match s {
            coll::Sized::DictEntry(d) => Ok(*d),
            c => c.into_iter().try_fit(),
        }
    }
}

impl Derive<Entry> for assoc::Associative {
    fn derive(d: Entry) -> assoc::Associative {
        let mut assoc = assoc::Association::fresh();
        let a = assoc.mutate();
        d.examples.and_then(|l| a.insert(fit!("examples"), l.fit()));
        d.spec.and_then(|l| a.insert(fit!("spec"), l.fit()));
        d.doc.and_then(|l| a.insert(fit!("doc"), l.fit()));
        a.insert(fit!("namespace"), d.namespace.as_ref().clone().fit());

        if let Executable::Derived(d) = d.definition {
            a.insert(fit!("definition"), crate::compile::decompile(&d).fit());
        }

        assoc::Associative::Assoc(assoc)
    }
}

impl TryDerive<Item> for Words {
    fn try_derive(i: Item) -> Result<Self, Error> {
        let s = coll::Sized::try_derive(i)?;
        match s {
            coll::Sized::Words(d) => Ok(d),
            c => c.into_iter().try_fit(),
        }
    }
}

impl TryDerive<Item> for Dictionary {
    fn try_derive(i: Item) -> Result<Self, Error> {
        let s = assoc::Associative::try_derive(i)?;
        match s {
            assoc::Associative::Dictionary(d) => Ok(*d),
            //assoc::Associative::Assoc(a) => a.into_iter().try_fit(),
            a => Err(Error::expected(fit!("dictionary"), a)),
        }
    }
}

impl Derive<Entry> for Item {
    fn derive(e: Entry) -> Self {
        Item::DictEntry(Box::new(e))
    }
}

impl Derive<Words> for Item {
    fn derive(e: Words) -> Self {
        Item::Words(e)
    }
}

impl Derive<Dictionary> for Item {
    fn derive(d: Dictionary) -> Self {
        Item::Dictionary(Box::new(d))
    }
}

impl Derive<(Word, Entry)> for Item {
    fn derive((k, v): (Word, Entry)) -> Item {
        list![k, v].fit()
    }
}

impl TryDerive<Item> for (Word, Entry) {
    fn try_derive(i: Item) -> Result<Self, Error> {
        let s = coll::Sized::try_derive(i)?;
        if s.count() != 2 {
            Err(Error::expected(fit!("pair"), s))
        } else {
            let mut iter = s.into_iter();
            let key: types::Word = iter.next().unwrap().try_fit()?;
            let value: Entry = iter.next().unwrap().try_fit()?;
            Ok((key.fit(), value))
        }
    }
}

// impl ToIterator for Words {
//     type Item = assoc::Entry;
//     type IntoIter = Box<dyn Iterator<Item = assoc::Entry>>;

//     fn to_iter<'a>(self) -> Self::IntoIter {
//         let items: Vec<_> = self
//             .iter()
//             .map(|(k, v)| (assoc::KeyItem::Word(k.clone().fit()), v.clone().fit()))
//             .collect();
//         Box::new(items.into_iter())
//     }
// }
