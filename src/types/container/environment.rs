//! Functionality of a kcats execution environment.
use super::{associative as assoc, dictionary as dict};
use crate::axiom;
use crate::derivation::*;
use crate::list;
use crate::serialize;
use crate::traits::*;
use crate::types::container::dictionary::Dict;
use crate::types::container::program::Program;
use crate::types::container::{self as coll};
use crate::types::*;

use once_cell::sync::Lazy;
use std::convert::Infallible;

/// A struct to hold the state of an executing kcats program. The
/// `stack` is the data being manipulated, the `program` is program
/// remaining to be executed, and the `dictionary` is the set of
/// functions available to the program.
#[derive(Clone, PartialEq)]
pub struct Environment {
    pub stack: coll::stack::Stack,
    pub program: Program,
    pub dictionary: dict::Dictionary,
}

impl Environment {
    /// Push the [Item] onto the top of the stack.
    pub fn push<T: Fit<Item>>(&mut self, i: T) {
        self.stack.push_front(i.fit());
    }

    pub fn push_err(&mut self, mut err: Error) {
        let trace = self.program.stacktrace();
        err.add_trace(trace);
        self.stack.push_front(err.fit());
    }

    /// Pop the top [Item] from the stack, panicking if the stack is
    /// empty.
    pub fn pop(&mut self) -> Item {
        self.stack.pop_front().unwrap()
    }

    /// Returns a reference to the top stack [Item], or [None] if it's
    /// empty.
    pub fn tos(&self) -> Option<&Item> {
        self.stack.front()
    }

    /// Returns the length of this struct (as an associative
    /// structure), which is constant.
    pub fn len(&self) -> usize {
        3 // 3 fields
    }

    pub fn is_empty(&self) -> bool {
        false
    }

    /// Treats the [Environment] as an associative structure,
    /// returning one of its fields, or [None].
    pub fn get(&self, key: &assoc::KeyItem) -> Option<Item> {
        match key {
            assoc::KeyItem::Word(w) => match w.data.as_str() {
                "stack" => Some(self.stack.to_list().fit()),
                "program" => Some(self.program.clone().fit()),
                "dictionary" => Some(self.dictionary.clone().fit()),
                _ => None,
            },
            _ => None,
        }
    }

    /// Returns true if the [Environment] contains the given key,
    /// which is only true for its fixed fields.
    pub fn contains_key(&self, key: &assoc::KeyItem) -> bool {
        Word::try_derive(key.clone())
            .is_ok_and(|ref w| matches!(w.fit(), "stack" | "program" | "dictionary"))
    }

    /// Reads a stdlib module and updates the dictionary.
    pub fn load_builtin_module(&mut self, module_alias: Word) -> Result<(), Error> {
        self.push(module_alias);
        axiom::read_blob(self)
    }

    /// Loads the core modules as part of preparing a standard
    /// environment.
    fn load_core_modules(&mut self) -> Result<(), Error> {
        // Assuming /project/core/ is in your project's root directory and part of the source
        let files: Vec<&[u8]> = vec![
            include_bytes!("../../kcats/core/stack-builtins.kcats"),
            include_bytes!("../../kcats/core/motion-builtins.kcats"),
            include_bytes!("../../kcats/core/compare-builtins.kcats"),
            include_bytes!("../../kcats/core/math-builtins.kcats"),
            include_bytes!("../../kcats/core/boolean-builtins.kcats"),
            include_bytes!("../../kcats/core/serialize-builtins.kcats"),
            include_bytes!("../../kcats/core/encode-builtins.kcats"),
            include_bytes!("../../kcats/core/strings-builtins.kcats"),
            include_bytes!("../../kcats/core/errors-builtins.kcats"),
            include_bytes!("../../kcats/core/pipes-builtins.kcats"),
            include_bytes!("../../kcats/core/collections-builtins.kcats"),
            include_bytes!("../../kcats/core/execute-builtins.kcats"),
            include_bytes!("../../kcats/core/dictionary-builtins.kcats"),
            include_bytes!("../../kcats/core/associations-builtins.kcats"),
            include_bytes!("../../kcats/core/environment-builtins.kcats"),
            include_bytes!("../../kcats/core/sets-builtins.kcats"),
            include_bytes!("../../kcats/stdlib/stack.kcats"),
            include_bytes!("../../kcats/stdlib/motion.kcats"),
            include_bytes!("../../kcats/stdlib/execute.kcats"),
            include_bytes!("../../kcats/stdlib/math.kcats"),
            include_bytes!("../../kcats/stdlib/encode.kcats"),
            include_bytes!("../../kcats/stdlib/compare.kcats"),
            include_bytes!("../../kcats/stdlib/collections.kcats"),
            include_bytes!("../../kcats/stdlib/associations.kcats"),
            include_bytes!("../../kcats/stdlib/dictionary.kcats"),
            include_bytes!("../../kcats/stdlib/environment.kcats"),
        ];

        for &file_contents in &files {
            let module =
                edn_format::canonicalize(String::from_utf8_lossy(file_contents).into_owned())?;
            match self.dictionary.words.insert_core_module(module.clone()) {
                Ok(namespace) => {
                    self.dictionary.modules.push(namespace);
                }
                Err(mut e) => {
                    e.push(fit!("content"), module.fit());
                    return Err(e);
                }
            }
        }
        Ok(())
    }

    /// Returns an error if the stack isn't at least `min_depth` deep.
    fn check_stack_depth(&self, min_depth: usize) -> Result<(), Error> {
        //println!("Checking stack has at least {} items", min_depth);
        if self.stack.len() < min_depth {
            Err(Error::stack_underflow())
        } else {
            Ok(())
        }
    }

    /// Returns an error if the stack doesn't match the given input
    /// spec. Gives helpful feedback where possible
    pub fn check_input_spec(&self, specs: &dict::StackSpec) -> Result<(), Error> {
        self.check_stack_depth(specs.len())?;

        // Fast path: check lazily without allocating
        let all_ok = specs.iter().enumerate().all(|(i, spec)| {
            let item = self.stack.get(i).unwrap();
            check_type(item, &spec.elemtype)
        });

        if all_ok {
            Ok(())
        } else {
            // Slow path: construct error
            let actual: Vec<Item> = specs
                .iter()
                .enumerate()
                .map(|(i, spec)| {
                    let item = self.stack.get(i).unwrap();
                    let pass = check_type(item, &spec.elemtype);
                    Item::derive(list!(spec.clone(), list!(pass, item.clone())))
                })
                .collect();

            Err(Error::expected(fit!("match-input-spec?"), actual))
        }
    }

    pub fn is_finished(&self) -> bool {
        self.program.is_empty()
    }

    /// A reducing function that loads modules one at a time. Takes an
    /// existing env, loads the given module and returns the new env with
    /// the modified dictionary that the module built.
    pub fn load_module(mut self, module: Item, is_using: bool) -> Environment {
        //println!("Loading module {:?}", module);
        //println!("Cache: {:?}", env.dictionary.lingo);
        self.push(module.clone());
        self.push(is_using);
        //println!("Loading module (should be empty?): {:?}", self.program);
        self.program
            .prepend(list!("dictionary", "⚓", "pair", "🎁", "using"));
        self = futures::executor::block_on(async move { axiom::eval(self).await });
        let item = self.pop();
        let mut dict: dict::Dictionary = match item.clone().try_fit() {
            Ok(d) => d,
            Err(_) => {
                panic!("Failed to fit Dictionary. Item was: {:?}", item);
            }
        };
        dict.resolve();
        self.dictionary = dict;
        //println!("loaded module {:?}: {:?}", module, self.dictionary);

        self
    }
}

impl Default for Environment {
    /// functions. The environment is only built once and memoized.
    fn default() -> Self {
        static INST: Lazy<Environment> = Lazy::new(|| {
            //println!("Env::default");
            let mut env = Environment {
                dictionary: dict::Dictionary {
                    words: dict::Words::builtins(),
                    modules: Default::default(),
                    lingo: dict::Words::fresh(),
                },
                stack: Default::default(),
                program: Default::default(),
            };

            env.load_core_modules()
                .expect("failed to load core modules");
            // add core module as default module

            env.dictionary.resolve();
            //println!("starting dictionary: {:?}", env.dictionary.modules);
            //println!("Dict has {} words", env.dictionary.words.len());
            let mut env = [
                fit!("errors"),
                fit!("encode"),
                fit!("time"), // good candidate for lib
                fit!("pipes"),
                fit!("methods"),
                fit!("generators"),
                fit!("debug"),
                fit!("crypto-builtins"),
                //fit!("more-generators"),
                //fit!("database"),
            ]
            .into_iter()
            .fold(env, |e, item| e.load_module(item, true));

            env = [
                fit!("collections"), // actually we probably want collections in default dictionary? wait maybe not
            ]
            .into_iter()
            .fold(env, |e, item| e.load_module(item, false));
            // need to do this again because we loaded some builtins
            // above, need to add the rust definitions back that got
            // overwritten.
            env.dictionary.resolve();

            //print!("Env: {:?}", env);
            env
        });
        INST.clone()
    }
}

/// Returns false if the [Item] is not of the type specified by
/// [Word] `w`. This allows specs to have their own little type
/// hierarchy, eg, `integer` is a `number`, `list` is a `sized` etc.
fn check_type(i: &Item, w: &Word) -> bool {
    //println!("Check {:?} is {:?}", w, i);
    //if *w == *S_DISPENSER {
    //   println!("Dispenser? {:?}", i);
    //}
    match (w, i) {
        (w, _) if *w == *S_ITEM => true,
        (
            w,
            Item::Standard
            | Item::Handoff(_)
            | Item::Socket(_)
            | Item::ServerSocket(_)
            | Item::StaticFile(_)
            | Item::Time
            | Item::Timer(_)
            | Item::String(_)
            | Item::Bytes(_)
            | Item::Dictionary(_)
            | Item::DictEntry(_)
            | Item::Assoc(_)
            | Item::Error(_)
            | Item::Words(_)
            | Item::Env(_)
            | Item::Nothing
            | Item::List(_)
            | Item::Program(_)
            | Item::Set(_),
        ) if *w == *S_DISPENSER => true,
        (
            w,
            Item::Standard
            | Item::Handoff(_)
            | Item::Socket(_)
            | Item::StaticFile(_)
            | Item::String(_)
            | Item::Bytes(_)
            | Item::Dictionary(_)
            | Item::DictEntry(_)
            | Item::Assoc(_)
            | Item::Error(_)
            | Item::Words(_)
            | Item::Env(_)
            | Item::Nothing
            | Item::List(_)
            | Item::Program(_)
            | Item::Set(_),
        ) if *w == *S_RECEPTACLE => true,
        (w, Item::Int(_)) if *w == *S_INTEGER || *w == *S_NUMBER => true,
        (w, Item::Float(_)) if *w == *S_FLOAT || *w == *S_NUMBER => true,
        (w, Item::Char(_)) if *w == *S_CHAR => true,
        // TODO: also handle cases where bytes/string is a list
        (w, Item::Bytes(_)) if *w == *S_BYTES || *w == *S_ORDERED => true,

        (w, Item::String(_)) if *w == *S_STRING => true,
        (w, Item::Word(_)) if *w == *S_WORD => true,

        (
            w,
            Item::Standard
            | Item::Handoff(_)
            | Item::Socket(_)
            | Item::ServerSocket(_)
            | Item::StaticFile(_)
            | Item::Time
            | Item::Timer(_),
        ) if *w == *S_PIPE => true,

        (w, Item::List(_) | Item::Program(_)) if *w == *S_LIST || *w == *S_PROGRAM => true,

        (
            w,
            Item::Dictionary(_)
            | Item::DictEntry(_)
            | Item::Assoc(_)
            | Item::Error(_)
            | Item::Words(_)
            | Item::Env(_)
            | Item::Nothing,
        ) if *w == *S_ASSOC => true,

        (w, Item::Error(_)) if *w == *S_ERROR => true,

        (w, Item::Dictionary(_)) if *w == *S_DICTIONARY => true,

        (
            w,
            Item::String(_)
            | Item::Bytes(_)
            | Item::Dictionary(_)
            | Item::DictEntry(_)
            | Item::Assoc(_)
            | Item::Error(_)
            | Item::Words(_)
            | Item::Env(_)
            | Item::Nothing
            | Item::List(_)
            | Item::Program(_)
            | Item::Set(_),
        ) if *w == *S_SIZED => true,

        (w, Item::String(_) | Item::Bytes(_) | Item::List(_) | Item::Program(_))
            if *w == *S_ORDERED =>
        {
            true
        }

        (w, Item::Env(_)) if *w == *S_ENVIRONMENT => true,
        (_, _) => false,
    }
}

impl TryDerive<Box<dyn Iterator<Item = Item>>> for Environment {
    fn try_derive(iter: Box<dyn Iterator<Item = Item>>) -> Result<Self, Error> {
        let mut stack: Option<coll::List> = None;
        let mut program: Option<Program> = None;
        let mut dictionary: Option<dict::Dictionary> = None;
        for i in iter {
            let (k, v): (assoc::KeyItem, Item) = i.try_fit()?;
            if k == fit!("stack") {
                stack = Some(v.try_fit()?)
            } else if k == fit!("program") {
                program = Some(v.try_fit()?)
            } else if k == fit!("dictionary") {
                let mut d = dict::Dictionary::try_derive(v)?;
                d.resolve();
                dictionary = Some(d);
            } else {
                continue;
            }
        }

        let env = Environment {
            stack: coll::stack::StackData::from_list(stack.unwrap_or_default()),
            program: program.unwrap_or_default(),
            dictionary: dictionary.unwrap_or_else(|| Environment::default().dictionary),
        };
        Ok(env)
    }
}

impl TryDerive<Item> for Environment {
    fn try_derive(i: Item) -> Result<Self, Error> {
        //println!("Convert to env: {:?}", i);
        let s = coll::Sized::try_derive(i)?;

        match s {
            coll::Sized::Env(e) => Ok(*e),
            l => l.into_iter().try_fit(),
        }
    }
}

impl TryDerive<(Item, dict::Dictionary)> for Environment {
    fn try_derive((i, d): (Item, dict::Dictionary)) -> Result<Self, Error> {
        let s = coll::Sized::try_derive(i)?;

        match s {
            coll::Sized::Env(e) => Ok(*e),
            coll::Sized::List(l) => {
                let mut env = Environment {
                    dictionary: d,
                    ..Environment::default()
                };
                let chunk = crate::compile::compile_with_dict(&l, Some(&env.dictionary));
                env.program.push_frame(crate::types::container::program::Frame {
                    chunk: std::sync::Arc::new(chunk),
                    ip: 0,
                    loop_counters: vec![],
                });
                Ok(env)
            }
            coll::Sized::Program(p) => {
                let mut env = Environment {
                    dictionary: d,
                    ..Environment::default()
                };
                env.program = p;
                Ok(env)
            }
            l => {
                let env: Environment = l.into_iter().try_fit()?;
                Ok(env)
            }
        }
    }
}

impl Derive<Environment> for Item {
    fn derive(env: Environment) -> Item {
        assoc::Associative::Env(Box::new(env)).fit()
    }
}

impl Derive<Environment> for Sometime<'static, Environment> {
    fn derive(env: Environment) -> Sometime<'static, Environment> {
        Sometime::Now(env)
    }
}

impl IntoIterator for Environment {
    type Item = assoc::Entry;
    type IntoIter = Box<dyn Iterator<Item = assoc::Entry>>;

    fn into_iter(self) -> Self::IntoIter {
        let v: Vec<(assoc::KeyItem, Item)> = vec![
            (fit!("stack"), self.stack.to_list().fit()),
            (fit!("program"), self.program.fit()),
            (fit!("dictionary"), self.dictionary.fit()),
        ];
        Box::new(v.into_iter())
    }
}

impl serialize::Display for Environment {
    fn representation(&self) -> Item {
        let a: assoc::AssociationContent = self.clone().rewrap();
        Arc::new(a).fit()
    }
}

/// When joining two identically shaped map-like structs, it's just
/// last one wins.
impl coll::Join<Environment> for Environment {
    type Output = Environment;
    type Error = Infallible;
    fn join(self, other: Self) -> Result<Self::Output, Self::Error> {
        Ok(other)
    }
}
