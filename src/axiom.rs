//! All the core functions of kcats: Words that are implemented in
//! rust, instead of in terms of other kcats words.
use crate::serialize::{self, Emit};
use crate::traits::*;
use crate::{fit, list};

use crate::config;
use crate::derivation::*;
#[cfg(feature = "database")]
use crate::types::container::pipe::db;
use crate::types::container::program::Program;
use crate::types::container::{
    self as coll, associative as assoc, dictionary as dict, environment::Environment, error::Error,
    Container, Count, Join, Mutey, Ordered, Take,
};
use crate::types::number::{Float, Int, Number};
use crate::types::*;
use cache::cache;
use dynfmt::{Format, SimpleCurlyFormat};
use futures::future::FutureExt;
use im::vector;
use internment::Intern;
use lazy_static::lazy_static;
use std::cmp::max;
use std::collections::HashMap;
use std::convert::Infallible;
use std::default::Default;
use std::mem;
use std::str::Utf8Error;
use std::sync::Arc;
//#[cfg(feature = "httpclient")]
//use surf;

pub type ItemResult = Result<Item, Error>;

/// Convert results into Items, for use when we intend to put the
/// result on the stack whether it's an [Error] or some other [Item].
impl<T, U> Derive<Result<T, U>> for Item
where
    T: Fit<Item>,
    U: Fit<Item>,
{
    fn derive(i: Result<T, U>) -> Self {
        match i {
            Ok(i) => i.fit(),
            Err(e) => e.fit(),
        }
    }
}

/// A higher order function that executes a simpler function `f`,
/// where `f` takes a stack item and returns a [Result] of another
/// stack item.
fn f_stack1<F, Arg1, Output, Err>(f: F) -> impl Fn(&mut Environment) -> StepResult
where
    F: Fn(Arg1) -> Result<Output, Err> + 'static,
    Arg1: TryDerive<Item> + Fit<Item> + Clone,
    //ArgMode::Detail<Item>: Fit<Error>,
    Err: Fit<Error>,
    //for<'a> <SpecInput as TryDerive<&'a Item>>::Error: Fit<Error>,
    Output: Fit<Item>,
{
    move |env: &mut Environment| {
        let x = env.stack.pop_front().ok_or_else(Error::stack_underflow);
        match x {
            Ok(i) => match <Arg1>::try_derive(i) {
                Ok(i) => {
                    let res = f(i);
                    match res {
                        Ok(r) => {
                            
                            //env.pop();
                            env.push(r);
                        }
                        Err(e) => {
                            env.push_err(e.fit());
                        }
                    }
                }
                Err(e) => {
                    env.push_err(e.fit());
                }
            },
            Err(e) => {
                env.push_err(e);
            }
        }
        StepResult::Done
    }
}

/// A higher order function that executes a simpler function `f`,
/// where `f` takes two stack items and returns a [Result] of another
/// stack item.
fn f_stack2<F, Output, SpecInputY, SpecInputX, E>(
    f: F,
) -> impl Fn(&mut Environment) -> StepResult
where
    F: Fn(SpecInputY, SpecInputX) -> Result<Output, E> + 'static,
    SpecInputX: for<'a> TryDerive<&'a Item> + Fit<Item> + Clone,
    //for<'a> <SpecInputX as TryDerive<&'a Item>>::Error: Fit<Error>,
    SpecInputY: for<'b> TryDerive<&'b Item> + Fit<Item> + Clone,
    //for<'b> <SpecInputY as TryDerive<&'b Item>>::Error: Fit<Error>,
    E: Fit<Error>,
    Output: Fit<Item>,
{
    move |env: &mut Environment| {
        let x = env
            .tos()
            .ok_or_else(Error::stack_underflow)
            .and_then(|x| SpecInputX::try_derive(x).map_err(Fit::fit));
        let y = env
            .stack
            .get(1)
            .ok_or_else(Error::stack_underflow)
            .and_then(|y| SpecInputY::try_derive(y).map_err(Fit::fit));

        match (x, y) {
            (Ok(x), Ok(y)) => {
                let res = f(y, x);
                match res {
                    Ok(r) => {
                        
                        env.stack.replace2(r.fit());
                    }
                    Err(e) => {
                        env.push_err(e.fit());
                    }
                }
            }
            (Err(e), _) => {
                env.push_err(e);
            }
            (_, Err(e)) => {
                env.push_err(e);
            }
        }
        StepResult::Done
    }
}

/// A higher order function that executes a simpler function `f`,
/// where `f` takes 3 stack items and returns a [Result] of another
/// stack item.
fn f_stack3<F, Output, SpecInputZ, SpecInputY, SpecInputX, E>(
    f: F,
) -> impl Fn(&mut Environment) -> StepResult
where
    F: Fn(SpecInputZ, SpecInputY, SpecInputX) -> Result<Output, E> + 'static,
    SpecInputX: for<'a> TryDerive<&'a Item> + Fit<Item> + Clone,
    //for<'a> <SpecInputX as TryDerive<&'a Item>>::Error: Fit<Error>,
    SpecInputY: for<'b> TryDerive<&'b Item> + Fit<Item> + Clone,
    //for<'b> <SpecInputY as TryDerive<&'b Item>>::Error: Fit<Error>,
    SpecInputZ: for<'c> TryDerive<&'c Item> + Fit<Item> + Clone,
    //for<'c> <SpecInputZ as TryDerive<&'c Item>>::Error: Fit<Error>,
    E: Fit<Error>,
    Output: Fit<Item>,
{
    move |env: &mut Environment| {
        let x = env
            .tos()
            .ok_or_else(Error::stack_underflow)
            .and_then(|x| SpecInputX::try_derive(x).map_err(Fit::fit));
        let y = env
            .stack
            .get(1)
            .ok_or_else(Error::stack_underflow)
            .and_then(|y| SpecInputY::try_derive(y).map_err(Fit::fit));
        let z = env
            .stack
            .get(2)
            .ok_or_else(Error::stack_underflow)
            .and_then(|z| SpecInputZ::try_derive(z).map_err(Fit::fit));
        match (x, y, z) {
            (Ok(x), Ok(y), Ok(z)) => {
                let res = f(z, y, x);
                match res {
                    Ok(r) => {
                        
                        env.stack.replace3(r.fit());
                    }
                    Err(e) => {
                        env.push_err(e.fit());
                    }
                }
            }
            (Err(e), _, _) => {
                env.push_err(e);
            }
            (_, Err(e), _) => {
                env.push_err(e);
            }
            (_, _, Err(e)) => {
                env.push_err(e);
            }
        }
        StepResult::Done
    }
}

fn f_stack2_async(
    f: fn(Item, Item) -> Sometime<'static, ItemResult>,
) -> impl Fn(&mut Environment) -> StepResult {
    move |env: &mut Environment| {
        let x = env.pop();
        let y = env.pop();
        match f(x, y) {
            Sometime::Now(r) => {
                if r.is_ok() {
                    
                }
                env.push(r);
                StepResult::Done
            }
            Sometime::Future(r) => {
                let mut env_owned = std::mem::replace(env, Environment::empty());
                StepResult::Async(Box::pin(r.map(move |r| {
                if r.is_ok() {
                    
                }
                env_owned.push(r);
                env_owned
            })))
            }
        }
    }
}

/// Wrapper function that allows you to use the ? operator in your own
/// functions. If that function returns an error result, it will
/// append that error to the env. The function `f` should return
/// either unit or an Error. If it returns an [Error] it will be
/// pushed onto the stack.
fn f_result<F>(f: F) -> impl Fn(&mut Environment) -> StepResult
where
    F: Fn(&mut Environment) -> Result<(), Error>,
{
    move |env: &mut Environment| {
        let r = f(env);
        match r {
            Ok(_) => StepResult::Done,
            Err(e) => {
                env.push_err(e);
                StepResult::Done
            }
        }
    }
}

/// A wrapper function that adapts regular 1-arity functions that
/// can't fail, into one that returns Result
fn ok1<I, R>(f: fn(I) -> R) -> impl Fn(I) -> Result<R, Infallible> {
    move |i| Ok(f(i))
}

fn ok2<I, J, R>(f: fn(I, J) -> R) -> impl Fn(I, J) -> Result<R, Infallible> {
    move |i, j| Ok(f(i, j))
}

fn ok3<I, J, K, R>(f: fn(I, J, K) -> R) -> impl Fn(I, J, K) -> Result<R, Infallible> {
    move |i, j, k| Ok(f(i, j, k))
}

#[derive(Clone)]
pub struct Builtin {
    pub f: &'static StepFn,
    pub name: &'static str,
}

impl Derive<Builtin> for Item {
    fn derive(a: Builtin) -> Item {
        Item::Builtin(Box::new(a))
    }
}

lazy_static! {
    pub static ref BUILTIN_FUNCTIONS: HashMap<Word, dict::Executable> = {

    #![allow(unused_mut)]
    let mut entries: Vec<(&str, &'static StepFn)> = vec![
        ("*", Box::leak(Box::new(f_stack2(ok2(mult))))),
        ("+", Box::leak(Box::new(f_stack2(ok2(plus))))),
        ("get", Box::leak(Box::new(f_stack2(lookup)))),
        ("sort-indexed", Box::leak(Box::new(f_stack1(sort_by_key)))),
        ("-", Box::leak(Box::new(f_stack2(ok2(minus))))),
        ("/", Box::leak(Box::new(f_stack2(div)))),
        //("<", Box::leak(Box::new(f_stack2(lt)))),
        //("<=", Box::leak(Box::new(f_stack2(lte)))),
        ("=", Box::leak(Box::new(eq))),
        //(">", Box::leak(Box::new(f_stack2(gt)))),
        //(">=", Box::leak(Box::new(f_stack2(gte)))),
        ("abs", Box::leak(Box::new(f_stack1(ok1(abs))))),
        ("and", Box::leak(Box::new(f_stack2(ok2(and))))),
        ("animate", Box::leak(Box::new(animate))),
        ("assign", Box::leak(Box::new(f_stack3(assign)))),
        (
            "association",
            Box::leak(Box::new(f_stack1(|i: crate::types::Item| assoc::Associative::try_derive(i)))),
        ),
        (
            "association?",
            Box::leak(Box::new(f_stack1(ok1(is_association)))),
        ),
        (
            "attend",
            Box::leak(Box::new(f_stack1(crate::types::container::pipe::channel::select))),
        ),
        ("autoformat", Box::leak(Box::new(f_stack1(autoformat)))),
        ("↔️", Box::leak(Box::new(branch))),
        ("bytes?", Box::leak(Box::new(f_stack1(ok1(is_bytes))))),
        ("cache", Box::leak(Box::new(f_result(write_blob)))),
        (
            "character",
            Box::leak(Box::new(f_stack1(|i: crate::types::Item| Char::try_derive(i)))),
        ),
        ("👥", Box::leak(Box::new(clone))),
        ("contains?", Box::leak(Box::new(f_stack2(ok2(contains))))),
        ("ceiling", Box::leak(Box::new(f_stack1(ok1(ceiling))))),
        ("compare", Box::leak(Box::new(f_stack2(compare)))),
        ("📏", Box::leak(Box::new(f_stack1(ok1(count))))),
        ("trace", Box::leak(Box::new(f_stack1(get_trace)))),
        ("dec", Box::leak(Box::new(f_stack1(ok1(dec))))),
        ("decache", Box::leak(Box::new(f_result(read_blob)))),
        ("disassemble", Box::leak(Box::new(disassemble))),
       // ("decide", Box::leak(Box::new(decide))),
        ("decodejson", Box::leak(Box::new(f_stack1(decode_json)))),
        ("dictmerge", Box::leak(Box::new(f_result(dictmerge)))),
        ("difference", Box::leak(Box::new(f_stack2(difference)))),
        ("🪄", Box::leak(Box::new(dip))),
        ("dictionary", Box::leak(Box::new(dictionary))),

        ("🗑️", Box::leak(Box::new(drop))),
        ("emit", Box::leak(Box::new(f_stack1(ok1(emit))))),
        ("empty", Box::leak(Box::new(f_stack1(empty)))),
        ("empty?", Box::leak(Box::new(f_stack1(ok1(is_empty))))),
        ("encodeitem", Box::leak(Box::new(f_stack1(encode_item)))),
        ("encodejson", Box::leak(Box::new(f_stack1(encode_json)))),
        (
            "environment",
            Box::leak(Box::new(f_stack1(|i: crate::types::Item| Environment::try_derive(i)))),
        ),
        ("environment?", Box::leak(Box::new(f_stack1(ok1(is_environment))))),
        ("error?", Box::leak(Box::new(f_stack1(ok1(is_error))))),
        ("eval-step", Box::leak(Box::new(eval_step_outer))),
        ("evaluate", Box::leak(Box::new(evaluate))),
        ("even?", Box::leak(Box::new(f_stack1(ok1(is_even))))),
        ("🧦", Box::leak(Box::new(evert))),
        ("▶️", Box::leak(Box::new(execute))),
        ("exp", Box::leak(Box::new(f_stack2(exp)))),
        ("fail", Box::leak(Box::new(f_result(fail)))),
        (
            "file-in",
            Box::leak(Box::new(f_stack1(crate::types::container::pipe::fs::file_in))),
        ),
        (
            "file-out",
            Box::leak(Box::new(f_stack1(crate::types::container::pipe::fs::file_out))),
        ),
        ("finished?", Box::leak(Box::new(f_stack1(ok1(is_finished))))),
        //("first", Box::leak(Box::new(f_stack1(first)))),
        ("🛟", Box::leak(Box::new(float))),
        ("floor", Box::leak(Box::new(f_stack1(ok1(floor))))),
        ("format", Box::leak(Box::new(f_stack2(format)))),
        ("handle", Box::leak(Box::new(handle))),
        (
            "handoff",
            Box::leak(Box::new(crate::types::container::pipe::channel::handoff)),
        ),
        (
            "hashbytes",
            Box::leak(Box::new(f_stack1(crate::crypto::hash))),
        ),
        ("inc", Box::leak(Box::new(f_stack1(ok1(inc))))),
        ("integer?", Box::leak(Box::new(f_stack1(ok1(is_integer))))),
        ("intersection", Box::leak(Box::new(f_stack2(intersection)))),
        ("inspect", Box::leak(Box::new(f_stack1(ok1(inspect))))),
        ("🔗", Box::leak(Box::new(f_stack2(join)))),
        ("key", Box::leak(Box::new(f_stack1(crate::crypto::key)))),
        ("last", Box::leak(Box::new(f_stack1(ok1(last))))),
        ("lingo", Box::leak(Box::new(f_stack1(ok1(lingo))))),
        ("list?", Box::leak(Box::new(f_stack1(ok1(is_list))))),
        ("log", Box::leak(Box::new(f_stack2(log)))),
        //("🌀", Box::leak(Box::new(loop_))),
        ("mod", Box::leak(Box::new(f_stack2(mod_)))),
        ("☯️", Box::leak(Box::new(f_stack1(ok1(not))))),
        ("namespace", Box::leak(Box::new(f_stack2(ok2(namespace))))),
        ("number", Box::leak(Box::new(f_stack1(|i: crate::types::Item| Number::try_derive(i))))),
        ("number?", Box::leak(Box::new(f_stack1(ok1(is_number))))),
        ("odd?", Box::leak(Box::new(f_stack1(ok1(is_odd))))),
        ("or", Box::leak(Box::new(f_stack2(ok2(or))))),
        ("🎒", Box::leak(Box::new(f_result(pack)))),
        ("parse-edn", Box::leak(Box::new(f_result(serialize::parse_edn)))),
        ("parse-utf8", Box::leak(Box::new(f_result(serialize::parse_utf8)))),
        ("pop", Box::leak(Box::new(pop))),
        ("📮", Box::leak(Box::new(put))),
        ("pipe?", Box::leak(Box::new(f_stack1(ok1(is_pipe))))),
        ("quot", Box::leak(Box::new(f_stack2(div)))),
        (
            "random",
            Box::leak(Box::new(f_stack1(crate::crypto::random))),
        ),
        ("range", Box::leak(Box::new(f_stack3(ok3(range))))),
        ("read", Box::leak(Box::new(f_stack1(serialize::parse)))),
        (
            "receiver",
            Box::leak(Box::new(f_stack1(crate::types::container::pipe::channel::receiver))),
        ),

        ("🪆", Box::leak(Box::new(f_recur))),
        ("remove", Box::leak(Box::new(f_stack2(remove)))),
        ("resolve", Box::leak(Box::new(resolve))),
        ("reverse", Box::leak(Box::new(f_stack1(reverse)))),
        ("round", Box::leak(Box::new(f_stack1(ok1(round))))),
        (
            "sender",
            Box::leak(Box::new(f_stack1(crate::types::container::pipe::channel::sender))),
        ),
        (
            "serversocket",
            Box::leak(Box::new(f_stack2_async(crate::types::container::pipe::net::server_socket))),
        ),
        (
            "set",
            Box::leak(Box::new(f_stack1(|i: crate::types::Item| coll::Set::try_derive(i)))),
        ),
        ("set?", Box::leak(Box::new(f_stack1(ok1(is_set))))),
        ("sign", Box::leak(Box::new(f_stack2(crate::crypto::sign)))),
        ("⚓", Box::leak(Box::new(sink))),
        ("slice", Box::leak(Box::new(f_stack3(slice)))),
        ("📸", Box::leak(Box::new(snapshot))),
        (
            "socket",
            Box::leak(Box::new(f_stack2_async(crate::types::container::pipe::net::socket))),
        ),
        ("sqrt", Box::leak(Box::new(f_stack1(ok1(sqrt))))),
        ("standard", Box::leak(Box::new(standard))),
        ("string", Box::leak(Box::new(f_stack1(string)))),
        ("string?", Box::leak(Box::new(f_stack1(ok1(is_string))))),
        ("🔀", Box::leak(Box::new(swap))),
        ("•🔀", Box::leak(Box::new(swapdown))) ,
        ("••🔀", Box::leak(Box::new(swapdeep))),
        (
            "timer",
            Box::leak(Box::new(f_stack1(crate::types::container::pipe::channel::timer))),
        ),
        ("timestamps", Box::leak(Box::new(timestamps))),
        ("unassign", Box::leak(Box::new(f_stack2(unassign)))),
        ("unnamespace", Box::leak(Box::new(unnamespace))),
        ("📤", Box::leak(Box::new(take))),
        ("🍫", Box::leak(Box::new(unwrap))),
        ("⛏️", Box::leak(Box::new(f_result(unpack)))),
        (
            "verify",
            Box::leak(Box::new(f_stack3(crate::crypto::verify))),
        ),
        //("version", Box::leak(Box::new(f_stack2(version)))),
        ("word", Box::leak(Box::new(f_stack1(|i: crate::types::Item| Word::try_derive(i))))),
        ("word?", Box::leak(Box::new(f_stack1(ok1(is_word))))),
        ("🎁", Box::leak(Box::new(wrap))),
        ("xor", Box::leak(Box::new(f_stack2(xor)))),
        ("✅", Box::leak(Box::new(self_insert))),
        ("zero?", Box::leak(Box::new(f_stack1(ok1(is_zero))))),
    ];

    #[cfg(feature = "database")]
    {
        entries.push(("database", Box::leak(Box::new(f_stack2(db::query)))));
        entries.push(("persist", Box::leak(Box::new(f_stack1(db::insert_object)))));
    }
        HashMap::from_iter(entries.into_iter().map(|(s, f)| (Word::try_derive(s).unwrap(), dict::Executable::Axiom(Box::leak(Box::new(Builtin {name: s, f}))))))
    };
}

fn pair(i: Item, j: Item) -> Item {
    list!(i, j).fit()
}

pub fn plus(i: Number, j: Number) -> Number {
    i.add(j)
}

pub fn minus(i: Number, j: Number) -> Number {
    i.subtract(j)
}

pub fn mult(i: Number, j: Number) -> Number {
    i.multiply(j)
}

pub fn div(i: Number, j: Number) -> Result<Number, Error> {
    Number::divide(i, j).map(Number::derive)
}

pub fn mod_(i: Number, j: Number) -> Result<Number, Error> {
    Number::remainder(i, j).map(Number::derive)
}

pub fn floor(i: Number) -> Int {
    match i {
        Number::Int(i) => i,
        Number::Float(i) => i.floor() as Int,
    }
}

pub fn ceiling(i: Number) -> Int {
    match i {
        Number::Int(i) => i,
        Number::Float(i) => i.ceil() as Int,
    }
}

pub fn round(i: Number) -> Int {
    match i {
        Number::Int(i) => i,
        Number::Float(i) => i.round() as Int,
    }
}

pub fn exp(base: Number, exponent: Number) -> Result<Number, Error> {
    match (base, exponent) {
        (number::Number::Int(b), number::Number::Int(e)) => b
            .checked_pow(e as u32)
            .ok_or(Error::overflow())
            .map(Number::derive),
        (number::Number::Float(b), number::Number::Float(e)) => Ok(Number::derive(b.powf(e))),
        (number::Number::Float(b), number::Number::Int(e)) => Ok(Number::derive(b.powi(e as i32))),
        (number::Number::Int(b), number::Number::Float(e)) => {
            Ok(Number::derive((b as f64).powf(e)))
        }
    }
}

pub fn log(value: Int, base: Int) -> Result<Float, Error> {
    if base <= 1 {
        Err(Error::too_small(base, 1))
    } else if value <= 0 {
        Err(Error::too_small(value, 0))
    } else {
        let base = base as Float;
        let value = value as Float;
        Ok(value.log(base))
    }
}

pub fn inc(i: Int) -> Int {
    i + 1
}

pub fn dec(i: Int) -> Int {
    i - 1
}

pub fn is_zero(i: Number) -> bool {
    match i {
        Number::Int(i) => i == 0,
        Number::Float(i) => i == 0.0,
    }
}

pub fn is_empty(i: Item) -> bool {
    i.is_empty()
}

pub fn gt(i: Number, j: Number) -> bool {
    Number::gt(i, j)
}

pub fn lt(i: Number, j: Number) -> bool {
    Number::lt(i, j)
}

pub fn gte(i: Number, j: Number) -> bool {
    Number::gte(i, j)
}

pub fn lte(i: Number, j: Number) -> bool {
    Number::lte(i, j)
}

pub fn join(i: coll::Sized, j: coll::Sized) -> Result<coll::Sized, Error> {
    i.join(j)
}

pub fn put(env: &mut Environment) -> StepResult {
    let i = env.pop();
    match coll::Receptacle::try_derive(env.pop()) {
        Ok(p) => {
            let r = p.put(i);
            match r {
                Sometime::Now(p) => {
                    
                    env.push(Item::derive(p));
                    StepResult::Done
                }
                Sometime::Future(fu) => {
                let mut env_owned = std::mem::replace(env, Environment::empty());
                StepResult::Async(Box::pin(fu.map(move |f| {
                    match f {
                        Ok(p) => {
                            
                            env_owned.push(Item::derive(p))
                        }
                        Err(e) => env_owned.push(e),
                    };
                                    env_owned
            })))
            }
            }
        }

        Err(e) => {
            env.push_err(e);
            StepResult::Done
        }
    }
}

pub fn clone(env: &mut Environment) -> StepResult {
    let clone = env.tos().expect("stack spec guarantees presence").clone();
    
    env.push(clone);
    StepResult::Done
}

fn swap2(env: &mut Environment, offset: usize) -> StepResult {
    env.stack.swap(offset, offset + 1);
    StepResult::Done
}

pub fn swap(env: &mut Environment) -> StepResult {
    
    swap2(env, 0)
}

pub fn swapdown(env: &mut Environment) -> StepResult {
    
    swap2(env, 1)
}

pub fn swapdeep(env: &mut Environment) -> StepResult {
    
    swap2(env, 2)
}

pub fn sink(env: &mut Environment) -> StepResult {
    env.stack.swap(0, 2);
    env.stack.swap(0, 1);
    
    StepResult::Done
}

pub fn float(env: &mut Environment) -> StepResult {
    env.stack.swap(0, 2);
    env.stack.swap(1, 2);
    
    StepResult::Done
}

pub fn drop(env: &mut Environment) -> StepResult {
    env.pop();
    
    StepResult::Done
}

pub fn eq(env: &mut Environment) -> StepResult {
    let is_eq = {
        let i = env.stack.get(0).expect("stack spec guarantees presence");
        let j = env.stack.get(1).expect("stack spec guarantees presence");
        i == j
    };
    
    env.stack.replace2(is_eq.fit());
    StepResult::Done
}

pub fn count(i: coll::Sized) -> Int {
    i.count() as Int
}

pub fn is_string(i: Item) -> bool {
    matches!(i, Item::String(_))
}

pub fn is_bytes(i: Item) -> bool {
    matches!(i, Item::Bytes(_))
}

pub fn is_error(i: Item) -> bool {
    matches!(i, Item::Error(_),)
}

pub fn is_word(i: Item) -> bool {
    matches!(i, Item::Word(_))
}

pub fn is_environment(i: Item) -> bool {
    matches!(i, Item::Env(_))
}

pub fn is_pipe(i: Item) -> bool {
    matches!(
        i,
        Item::Handoff(_)
            | Item::Socket(_)
            | Item::ServerSocket(_)
            | Item::StaticFile(_)
            | Item::Standard
            | Item::Timer(_)
            | Item::Time
    )
}

pub fn is_number(i: Item) -> bool {
    Number::try_derive(i).is_ok()
}

pub fn is_integer(i: Item) -> bool {
    matches!(i, Item::Int(_))
}

pub fn is_list(i: Item) -> bool {
    coll::Sized::try_derive(i)
        .map(|s| matches!(s, coll::Sized::List(_)))
        .unwrap_or(false)
}

// pub fn first(c: coll::Sized) -> ItemResult {
//     let (_, i) = c.take();
//     Ok(i.fit())
// }

pub fn last(c: coll::Sized) -> Item {
    c.into_iter().last().unwrap_or_default()
}

impl Identity for Sometime<'static, Environment> {}

pub fn execute(env: &mut Environment) -> StepResult {
    let i = env.pop();
    if let Item::Builtin(b) = i {
        (*b.f)(env)
    } else {
        match Program::try_derive(i) {
            Ok(program) => {
                
                env.program.prepend_program(program);
            }
            Err(e) => {
                env.push(e);
            }
        }
        StepResult::Done
    }
}

pub fn wrap(env: &mut Environment) -> StepResult {
    let item = env.pop();
    
    env.push(list!(item));
    StepResult::Done
}

pub fn unwrap(env: &mut Environment) -> StepResult {
    //println!("Unwrap: {:?} {:?}", env.stack, env.program.stacktrace());
    match coll::List::try_derive(env.pop()) {
        Ok(l) => {
            
            for item in l.iter().cloned() {
                env.push(item);
            }
        }
        Err(e) => {
            env.push(e);
        }
    };
    StepResult::Done
}

/// If it's a word, don't bother wrapping and
/// unwrapping, just flag it as quoted, and the
/// evaluator will just push it unexamined.
fn dip_quote(i: &mut Item) {
    if let Item::Word(w) = i {
        let mut j = w.as_ref().clone();
        j.quoted = true;
        *i = Item::Word(Intern::new(j));
    }
}

pub fn get_trace(p: Program) -> Result<coll::List, Error> {
    Ok(p.stacktrace())
}

pub fn dip(env: &mut Environment) -> StepResult {
    match Program::try_derive(env.pop()) {
        Ok(program) => {
            let mut item = env.pop();
            
            dip_quote(&mut item);
            //println!("Dip item: {:?}", item);
            env.program.prepend(vector![item]);
            env.program.prepend_program(program);
            //println!("Dip: {:?}", env.program);
        }
        Err(e) => env.push_err(e),
    }
    StepResult::Done
}



pub fn take(env: &mut Environment) -> StepResult {
    // TODO: handle Nothing case
    fn finish(env: &mut Environment,
        i: Result<Option<Item>, Error>,
        c: coll::Dispenser,
    ) {
        env.push(c);
        env.push(coll::result_to_option(i).unwrap_or_default());
    }
    match coll::Dispenser::try_derive(env.pop()) {
        Ok(d) => match d.take() {
            Sometime::Now(r) => {
                let (i, c) = r;
                { finish(env, i, c); StepResult::Done }; StepResult::Done
            }
            Sometime::Future(r) => {
                let mut env_owned = std::mem::replace(env, Environment::empty());
                StepResult::Async(Box::pin(async move {
                    let (i, c) = r.await;
                    finish(&mut env_owned, i, c);
                    env_owned
                }))
            },
        },
        Err(e) => {
            //println!("Not a dispenser! {:?}", env.tos().unwrap());
            env.push_err(e);
            StepResult::Done
        }
    }
}

pub fn pop(env: &mut Environment) -> StepResult {
    match <coll::Sized as TryDerive<_>>::try_derive(env.pop()) {
        Ok(it) => {
            let (c, i) = it.pop();
            
            env.push(c);
            env.push(i.unwrap_or_default());
        }
        Err(e) => {
            env.push_err(e);
        }
    }
    StepResult::Done
}

pub fn is_truthy(i: &Item) -> bool {
    !i.is_empty()
}

pub fn branch(env: &mut Environment) -> StepResult {
    match (
        Program::try_derive(env.pop()),
        Program::try_derive(env.pop()),
    ) {
        (Ok(false_branch), Ok(true_branch)) => {
            
            //env.pop();
            //env.pop();
            let b = env.tos().expect("stack spec guarantees presence");

            let selected_branch = if is_truthy(b) {
                true_branch
            } else {
                // A falsey value is useless, so drop it
                env.pop();
                false_branch
            };
            env.program.prepend_program(selected_branch)
        }
        (Err(e), i) => {
            env.push(i);
            env.push_err(e);
        }
        (i, Err(e)) => {
            env.push(e.actual().expect("error actual guarantees presence"));
            env.push(i);
            env.push(e);
        }
    }
    StepResult::Done
}

pub fn step(env: &mut Environment) -> StepResult {
    fn finish(env: &mut Environment,
        r: Result<Option<Item>, Error>,
        dispenser: coll::Dispenser,
        p: coll::List,
    ) {
        if let Some(litem) = coll::result_to_option(r) {
            // prepare the next iteration. First we execute p, then we
            // push the remaining dispenser and a new copy of p. Do
            // this even if the dispenser is currently empty. step is
            // still the next instruction, so we don't pop it off.
            env.program.prepend(list![dispenser, p.clone()]);
            env.program.prepend(p);
            env.push(litem);
        } else {
            // if the container is empty, just pop off 'step' and we're done
            
        }
    }
    let p = coll::List::try_derive(env.pop()).expect("stack spec guarantees List");
    let dispenser =
        coll::Dispenser::try_derive(env.pop()).expect("stack spec guarantees Dispenser");
    match dispenser.take() {
        Sometime::Now((r, dispenser)) => { finish(env, r, dispenser, p); StepResult::Done },
        Sometime::Future(f) => {
            let mut env_owned = std::mem::replace(env, Environment::empty());
            StepResult::Async(Box::pin(async move {
                let (r, dispenser) = f.await;
                finish(&mut env_owned, r, dispenser, p);
                env_owned
            }))
        },
    }
}

pub fn range(from: Int, to: Int, stepby: Int) -> coll::List {
    //allow stepping backwards eg 20 15 -2 range = [20 18 16]
    // to = -20 from = -10 step -2
    if stepby < 0 {
        let mut v: coll::List = ((to - stepby)..(from + 1))
            .step_by(-stepby as usize)
            .rewrap();
        v.reverse();
        v
    } else {
        (from..to).step_by(stepby as usize).rewrap()
    }
}

// (effect [rec2 rec1 then pred]
//                   ['[if]
//[(concat rec1
//         [[pred then rec1 rec2 'recur]] rec2)
// then pred]])

//(fn [{[l & others] 'stack :as env}]
//            (assoc env 'stack (apply list (vec others) l)))

pub fn evert(env: &mut Environment) -> StepResult {
    let l = coll::List::try_derive(env.pop()).expect("stack spec guarantees List");
    let tmp = env.stack.to_list();
    env.stack = crate::types::container::stack::StackData::from_list(l);
    let l = tmp;
    
    env.push(l);
    StepResult::Done
}

pub fn snapshot(env: &mut Environment) -> StepResult {
    
    env.push(env.stack.to_list());
    StepResult::Done
}

fn assoc_in(i: Option<Item>, ks: &[assoc::KeyItem], v: Item) -> Result<Item, Error> {
    fn assoc_vec(mut l: coll::List, ks: &[assoc::KeyItem], k: Int, v: Item) -> Result<Item, Error> {
        let idx = k as usize;
        let current_len = l.len();

        // extend the size of the vector to be big enough
        if idx >= current_len {
            // Extend with copies of the value
            for _ in current_len..idx + 1 {
                l.push_back(Item::default());
            }
        }

        l[idx] = if ks.is_empty() {
            v
        } else {
            assoc_in(l.get(idx).cloned(), ks, v)?
        };

        Ok(l.fit())
    }

    fn assoc_map(
        a: assoc::Associative,
        ks: &[assoc::KeyItem],
        k: &assoc::KeyItem,
        v: Item,
    ) -> Result<Item, Error> {
        let inner = a.get(k).clone();
        if ks.is_empty() {
            Ok(a.insert(k.clone(), v).0.fit())
        } else {
            Ok(a.insert(k.clone(), assoc_in(inner, ks, v)?).0.fit())
        }
    }

    if let [k, ks @ ..] = ks {
        match (i, k) {
            // An int key for a list means update that index
            (Some(Item::List(l)), assoc::KeyItem::Int(k)) => assoc_vec(*l, ks, *k, v),

            // Where there was nothing at a given index/key, and a non-int
            // key, create a map
            (None, k) => assoc_map(
                assoc::Associative::Assoc(assoc::Association::fresh()),
                ks,
                k,
                v,
            ),

            (Some(i), assoc::KeyItem::Int(k)) => {
                // An int key for an associative means an integer key, which is uncommon
                // but we'll support it
                match assoc::Associative::try_derive(i.clone()) {
                    Ok(a) => assoc_map(a, ks, &assoc::KeyItem::Int(*k), v),
                    Err(_) => {
                        // An int key for a non-sized type means we're overwriting
                        // whatever it is with a list, with the value at that index
                        assoc_vec(coll::List::new(), ks, *k, v)
                    }
                }
            }

            // Whatever it is, treat it as a map if possible
            (Some(i), k) => {
                let a = assoc::Associative::try_derive(i)?;
                assoc_map(a, ks, k, v)
            }
        }
    } else {
        Ok(i.unwrap())
    }
}

/// TODO: support demotion properly when removing required fields from assoc
fn remove(i: coll::Sized, k: Item) -> Result<Item, Error> {
    match i {
        coll::Sized::List(mut l) => {
            if let Some(pos) = l.iter().position(|x| *x == k) {
                l.remove(pos);
            }

            Ok((*l).fit())
        }
        coll::Sized::Set(mut s) => {
            s.mutate().remove(&assoc::KeyItem::try_derive(k)?);
            Ok(s.fit())
        }
        coll::Sized::Assoc(mut a) => {
            let am = Arc::mutate(&mut a);
            let v = am.remove(&(k.try_fit()?));
            //(Associative::Assoc(a), v)
            Ok(v.fit())
        }
        coll::Sized::Words(mut d) => {
            let dm = Arc::mutate(&mut d);
            let v = dm.remove(&Word::try_derive(k.clone()).unwrap_or_default());
            //(Associative::Words(d), v.map(|v| v.fit()))
            Ok(v.fit())
        }
        coll::Sized::Error(mut e) => {
            let a = e.data.mutate();
            let v = a.remove(&(k.try_fit()?));
            //(Associative::Error(e), v)
            Ok(v.fit())
        }
        coll::Sized::Env(e) => {
            let a: assoc::Association = Arc::new((*e).rewrap());
            let (_, v) = assoc::Associative::Assoc(a).remove(&(k.try_fit()?));
            Ok(v.fit())
        }

        i => Err(Error::expected(fit!("simple-container"), i)),
    }
}

fn unassoc_in(i: Item, ks: &[assoc::KeyItem]) -> Result<Item, Error> {
    if let [k, ks @ ..] = ks {
        if ks.is_empty() {
            let a = <assoc::Associative as TryDerive<_>>::try_derive(i)?;
            Ok(a.remove(k).0.fit())
        } else {
            match (i, k) {
                (Item::List(mut l), assoc::KeyItem::Int(k)) => {
                    let old_value = if let Some(item) = l.get_mut(*k as usize) {
                        mem::take(item)
                    } else {
                        return Err(Error::short_list(*k));
                    };
                    let new_value = unassoc_in(old_value, ks)?;
                    l[*k as usize] = new_value;
                    Ok((*l).fit())
                }
                (a, k) => {
                    let a: assoc::Associative = a.try_fit()?;
                    let mut a: assoc::Association = Arc::new(a.rewrap());
                    let am = a.mutate();
                    let mut res: Option<Result<_, Error>> = None;
                    am.entry(k.clone()).and_modify(|v| {
                        let new_value = unassoc_in(v.clone(), ks);
                        res = Some(new_value.map(|nv| {
                            *v = nv;
                        }));
                    });
                    if let Some(Err(e)) = res {
                        return Err(e);
                    }
                    Ok(a.fit())
                }
            }
        }
    } else {
        Ok(i)
    }
}

pub fn assign(m: Item, ks: Item, v: Item) -> ItemResult {
    //println!("Assign! {:?}", m);
    let kit = coll::List::try_derive(ks).map_err(Error::derive)?;
    let ksvec: Vec<assoc::KeyItem> = kit
        .iter()
        .cloned()
        .map(assoc::KeyItem::try_derive)
        .collect::<Result<_, Error>>()
        .map_err(Error::derive)?;

    assoc_in(Some(m), &ksvec, v)
}

pub fn unassign(m: Item, ks: Item) -> ItemResult {
    let kit = coll::List::try_derive(ks).map_err(Error::derive)?;
    let ksvec: Vec<assoc::KeyItem> = kit
        .iter()
        .cloned()
        .map(assoc::KeyItem::try_derive)
        .collect::<Result<_, Error>>()
        .map_err(Error::derive)?;

    unassoc_in(m, &ksvec)
}

pub fn lookup(i: coll::Sized, k: assoc::KeyItem) -> ItemResult {
    //println!("lookup {:?} \n {:?}", i, k);
    //let k = assoc::KeyItem::try_derive(k)?;
    //let i = coll::Sized::try_derive(i)?;
    match (i, k) {
        (coll::Sized::List(l), assoc::KeyItem::Int(k)) => {
            Ok(l.get(k as usize).cloned().unwrap_or_default())
        }
        (coll::Sized::String(s), assoc::KeyItem::Int(k)) => {
            //let s = s.inner();
            s.chars()
                .nth(k as usize)
                .map_or(Ok(Item::default()), |c| Ok(c.fit()))
        }
        (coll::Sized::Bytes(b), assoc::KeyItem::Int(k)) => b
            .get(k as usize)
            .cloned()
            .map_or(Ok(Item::default()), |c| Ok((c as i64).fit())),
        (i, k) => {
            let m = assoc::Associative::try_derive(i)?;
            Ok(m.get(&k).unwrap_or_default())
        }
    }
}

pub fn contains(c: Item, i: Item) -> bool {
    match coll::Sized::try_derive(c) {
        Ok(c) => c.has(&i),
        Err(_) => false,
    }
}

pub fn or(i: Item, j: Item) -> Item {
    if is_truthy(&i) {
        i
    } else if is_truthy(&j) {
        j
    } else {
        Item::default()
    }
    //Ok(Item::derive(is_truthy(i) || is_truthy(j)))
}

pub fn and(i: Item, j: Item) -> Item {
    if is_truthy(&i) && is_truthy(&j) {
        j
    } else {
        Item::default()
    }
}

pub fn not(i: Item) -> bool {
    !is_truthy(&i)
}

pub fn is_association(i: Item) -> bool {
    matches!(i, Item::Assoc(_))
}

pub fn is_set(i: Item) -> bool {
    coll::Sized::try_derive(i)
        .map(|s| matches!(s, coll::Sized::Set(_)))
        .unwrap_or(false)
}

pub fn is_odd(i: Int) -> bool {
    i & 1 == 1
}

pub fn is_even(i: Int) -> bool {
    i & 1 == 0
}

pub fn emit(l: coll::List) -> String {
    l.iter().emit()
}

pub fn autoformat(i: Item) -> Result<String, Error> {
    let s = String::try_derive(i).map_err(Error::derive)?;
    Ok(serialize::auto_format(s.as_str(), 20, 80))
}

/// Inner function of the interpreter, each call to this function
/// advances the [Environment] one step of execution.
pub fn eval_step(env: &mut Environment) -> StepResult {
    env.program.clean();
    let op = env.program.0.last_mut().and_then(|f| f.next_op());
    if let Some(op) = op {
        match op {
            crate::types::container::program::Op::Push(item) => {
                env.push(item);
                StepResult::Done
            }
            crate::types::container::program::Op::Call(word) => {
                if word.quoted {
                    let mut w = word.as_ref().clone();
                    w.quoted = false;
                    env.push(w);
                    StepResult::Done
                } else {
                    let definition = {
                        if word.namespace.is_some() {
                            env.dictionary.words.get(&word)
                        } else {
                            env.dictionary.lingo.get(&word)
                        }
                    };
                    if let Some(dfn) = definition {
                        if let Some(spec) = &dfn.spec {
                            if let Err(e) = env.check_input_spec(&spec.0) {
                                env.push_err(e);
                                return StepResult::Done;
                            }
                        }

                        match &dfn.definition {
                            dict::Executable::Axiom(a) => (*a.f)(env),
                            dict::Executable::Derived(d) => {
                                env.program.push_frame(crate::types::container::program::Frame {
                                    chunk: d.clone(),
                                    ip: 0,
                                    loop_counters: vec![],
                                });
                                StepResult::Done
                            }
                        }
                    } else {
                        env.push_err(Error::undefined(word.clone().fit()));
                        StepResult::Done
                    }
                }
            }

            crate::types::container::program::Op::Shuffle { pops, pushes } => {
                if let Err(e) = env.stack.shuffle(pops, &pushes) {
                    env.push(e);
                }
                StepResult::Done
            }
            crate::types::container::program::Op::Execute(chunk) => {

                env.program.push_frame(crate::types::container::program::Frame {
                    chunk,
                    ip: 0,
                    loop_counters: vec![],
                });
                StepResult::Done
            }
            crate::types::container::program::Op::Dip(chunk) => {
                let item = env.pop();
                let push_chunk = Arc::new(crate::types::container::program::Chunk {
                    ops: vec![crate::types::container::program::Op::Push(item), crate::types::container::program::Op::Return],
                    source: None,
                });
                env.program.push_frame(crate::types::container::program::Frame {
                    chunk: push_chunk,
                    ip: 0,
                    loop_counters: vec![],
                });
                env.program.push_frame(crate::types::container::program::Frame {
                    chunk,
                    ip: 0,
                    loop_counters: vec![],
                });
                StepResult::Done
            }
            crate::types::container::program::Op::Jump(offset) => {
                let frame = env.program.0.last_mut().unwrap();
                frame.ip = (frame.ip as isize + offset) as usize;
                StepResult::Done
            }
            crate::types::container::program::Op::JumpIfFalse(offset) => {
                let item = env.pop();
                if !is_truthy(&item) {
                    let frame = env.program.0.last_mut().unwrap();
                    frame.ip = (frame.ip as isize + offset) as usize;
                }
                StepResult::Done
            }
            crate::types::container::program::Op::JumpIfFalseKeepIfTrue(offset) => {
                let is_true = is_truthy(env.stack.front().unwrap_or(&crate::types::Item::List(Box::new(crate::types::container::List::new()))));
                if !is_true {
                    env.pop();
                    let frame = env.program.0.last_mut().unwrap();
                    frame.ip = (frame.ip as isize + offset) as usize;
                }
                StepResult::Done
            }
            crate::types::container::program::Op::JumpIfTrue(offset) => {
                let item = env.pop();
                if is_truthy(&item) {
                    let frame = env.program.0.last_mut().unwrap();
                    frame.ip = (frame.ip as isize + offset) as usize;
                }
                StepResult::Done
            }
            crate::types::container::program::Op::PushLoopCounter => {
                env.program.0.last_mut().unwrap().loop_counters.push(0);
                StepResult::Done
            }
            crate::types::container::program::Op::PopLoopCounter => {
                env.program.0.last_mut().unwrap().loop_counters.pop();
                StepResult::Done
            }
            crate::types::container::program::Op::IncLoopCounter(idx) => {
                env.program.0.last_mut().unwrap().loop_counters[idx] += 1;
                StepResult::Done
            }
            crate::types::container::program::Op::DecLoopCounter(idx) => {
                env.program.0.last_mut().unwrap().loop_counters[idx] -= 1;
                StepResult::Done
            }
            crate::types::container::program::Op::JumpIfLoopCounterZero(idx, offset) => {
                let frame = env.program.0.last_mut().unwrap();
                if frame.loop_counters[idx] == 0 {
                    frame.ip = (frame.ip as isize + offset) as usize;
                }
                StepResult::Done
            }
            crate::types::container::program::Op::Return => {
                env.program.pop_frame();
                StepResult::Done
            }
        }
    } else {
        StepResult::Done
    }
}

fn reverse(s: coll::Sized) -> Result<coll::Sized, Error> {
    match s {
        coll::Sized::List(mut l) => Ok({
            l.reverse();
            (*l).fit()
        }),
        coll::Sized::String(s) => Ok(s.chars().rev().collect::<String>().fit()),
        coll::Sized::Bytes(b) => Ok(b.into_iter().rev().collect::<Vec<u8>>().fit()),
        s => Err(Error::expected(fit!("ordered"), s)),
    }
}

/// The default method of encoding items into byte arrays.
fn encode_item(i: Item) -> Result<Bytes, Infallible> {
    Ok(match i {
        Item::Int(i) => i.to_be_bytes().to_vec(),
        Item::Float(f) => f.to_be_bytes().to_vec(),
        Item::Char(c) => vec![c as u8],
        i => {
            let r = coll::Sized::try_derive(i.clone());
            match r {
                Ok(s) => match s {
                    coll::Sized::String(s) => s.as_bytes().to_vec(),
                    coll::Sized::Bytes(b) => b,
                    coll::Sized::List(l) => Bytes::try_derive((*l).clone())
                        .unwrap_or_else(|_| (&Item::List(l)).emit().as_bytes().to_vec()),
                    s => (&Item::derive(s)).emit().as_bytes().to_vec(),
                },
                Err(_) => (&i).emit().as_bytes().to_vec(),
            }
        }
    })
}

fn string(i: Item) -> Result<String, Error> {
    match coll::Sized::try_derive(&i) {
        Ok(s) => {
            if s.is_empty() {
                Ok("".to_string())
            } else {
                match s {
                    coll::Sized::Bytes(b) => Ok(std::str::from_utf8(&b)?.to_string()),
                    coll::Sized::List(l) => {
                        String::try_derive((*l).clone()).or_else(|_| Ok((&Item::derive(*l)).emit()))
                    }
                    s => Ok((&Item::derive(s)).emit()),
                }
            }
        }
        Err(_) => Ok(i.emit()),
    }
}

fn get_error(env: &Environment) -> Option<Error> {
    env.tos().and_then(|i| match i {
        Item::Error(e) => Some(e.clone()),
        _ => None,
    })
}

fn unwind(mut env: Environment) -> Environment {
    let err = env.pop();
    // TODO: handle Nothing case
    let handle: &Item = &fit!("handle");
    let err = match err {
        Item::Error(mut e) => {
            let data = e.data.mutate();
            let mut unwound: Program = data
                .remove(&fit!("unwound"))
                .unwrap_or_default()
                .try_fit()
                .unwrap_or_else(|_| Program::default());

            let (new_unwound, found_handle) = env.program.unwind_to_handle(handle);
            unwound.extend_program(new_unwound);

            if found_handle {
                // didn't unwind the whole program, handled error
                // set the is_handled bit
                e.is_handled = true;
            }
            let em = Arc::make_mut(&mut e.data);
            em.insert(fit!("unwound"), unwound.fit());
            e.fit()
        }
        i => i,
    };
    env.push(err);
    env
}

pub async fn eval(mut env: Environment) -> Environment {
    //println!("Eval: {:?}", env);
    loop {
        if let Some(err) = get_error(&env) {
            if !err.is_handled {
                env = unwind(env); // TODO: this should be done in eval_step
            };
        }
        if !env.program.is_empty() {
            env = match eval_step(&mut env) {
                StepResult::Done => env,
                StepResult::Async(fenv) => fenv.await,
            };
        } else {
            break;
        }
    }
    env
}

pub fn eval_step_outer(env: &mut Environment) -> StepResult {
    let tos = env.pop();
    let inner_env = Environment::try_derive((tos, env.dictionary.clone()));

    match inner_env {
        Ok(inner) => {
            
            if inner.program.is_empty() {
                env.push(Item::default());
                StepResult::Done
            } else {
                let mut inner_mut = inner;
                match eval_step(&mut inner_mut) {
                    StepResult::Done => {
                        env.push(inner_mut);
                        StepResult::Done
                    }
                    StepResult::Async(finner) => {
                    let mut env_owned = std::mem::replace(env, Environment::empty());
                    StepResult::Async(Box::pin(finner.map(move |e| {
                        env_owned.push(e);
                        env_owned
                    })))
                },
                }
            }
        }
        Err(e) => {
            env.push(e);
            StepResult::Done
        }
    }
}

pub fn evaluate(env: &mut Environment) -> StepResult {
    match Environment::try_derive((env.tos().expect("stack spec guarantees Environment").clone(), env.dictionary.clone())) {
        Ok(inner) => {
                let mut env_owned = std::mem::replace(env, Environment::empty());
                StepResult::Async(Box::pin(eval(inner).map(move |inner_done| {
            
            env_owned.pop();
                env_owned.push(inner_done);
                env_owned
            })))
            }
        Err(e) => {
            env.push(e);
            StepResult::Done
        }
    }
}

pub fn dictionary(env: &mut Environment) -> StepResult {
    //println!("adding dictionary");
    let d = env.dictionary.clone();
    
    env.push(d);
    StepResult::Done
}

pub fn lingo(dict: dict::Dictionary) -> dict::Words {
    dict.lingo
}

fn sqrt(i: Number) -> Number {
    i.sqrt()
}

fn abs(i: Number) -> Number {
    i.abs()
}

/// If there's an unhandled error on the stack, handle it, otherwise
/// no-op.
fn handle(env: &mut Environment) -> StepResult {
    
    if let Some(Item::Error(ref mut e)) = env.stack.front_mut() {
        e.is_handled = true;
    }
    StepResult::Done
}

pub fn self_insert(env: &mut Environment) -> StepResult {
    env.push(true);
    StepResult::Done
}

pub fn fail(env: &mut Environment) -> Result<(), Error> {
    let mut err = Error::try_derive(env.tos().expect("stack spec guarantees Error"))
        .map_err(Error::derive)?;
    err.is_handled = false;
    
    env.pop();
    env.push_err(err);
    Ok(())
}

/// Takes a dictionary diff, merges it into an existing dictionary,
/// with all the changes marked with the given namespace.
pub fn dictmerge(env: &mut Environment) -> Result<(), Error> {
    //println!("dictmerge: {:?}", env);
    let modified =
        dict::Dictionary::try_derive(env.tos().expect("stack spec guarantees Dictionary"))
            .map_err(Error::derive)?;
    let mut existing =
        dict::Dictionary::try_derive(env.stack.get(1).expect("stack spec guarantees Dictionary"))
            .map_err(Error::derive)?;
    let namespace =
        dict::Namespace::try_derive(env.stack.get(2).expect("stack spec guarantees Namespace"))
            .map_err(Error::derive)?;

    existing.merge(modified, &namespace);

    // pop the word dictmerge
    
    env.pop();
    env.pop();
    env.pop();
    env.push(existing);
    Ok(())
}

/// Fetches a binary blob from the cache. The top of stack should be
/// either the hash of the content or its alias (a [Word]).
pub fn read_blob(env: &mut Environment) -> Result<(), Error> {
    //println!("Env: {:?}", env);
    let cache = config::PlatformConfig::get()?.cache;
    let contents = match env.pop() {
        Item::Word(alias) => cache.get(&cache::Key::Alias(alias.fit()))?,
        i => {
            let hash = Bytes::try_derive(i).map_err(Error::derive)?;
            cache.get(&cache::Key::Hash(hash))?
        }
    };
    
    env.push(contents);
    Ok(())
}

pub fn disassemble(env: &mut Environment) -> StepResult {
    let item = env.pop();
    let string_repr = match item {
        Item::List(l) => {
            let chunk = crate::compile::compile_with_dict(&l, Some(&env.dictionary));
            format!("{:?}", chunk.ops)
        }
        Item::Word(w) => {
            let dfn = env.dictionary.get_entry(&w);
            if let Some(entry) = dfn {
                match entry.definition {
                    dict::Executable::Axiom(_) => format!("Axiom(<native code>)"),
                    dict::Executable::Derived(chunk) => format!("{:?}", chunk.ops),
                }
            } else {
                format!("Undefined word")
            }
        }
        _ => format!("Cannot disassemble {:?}", item),
    };
    env.push(Item::String(Box::new(string_repr)));
    StepResult::Done
}

/// Writes a given binary object to the cache. Supports [Bytes], and
/// certain kinds of pipes. The top of stack should be the alias to
/// store the contents under, which should be either a [Word] or
/// nothing. If nothing, the object will only be available via its
/// hash. Returns the hash.
pub fn write_blob(env: &mut Environment) -> Result<(), Error> {
    let alias = match env.pop() {
        Item::Word(w) => Some(w.fit()),
        _ => None,
    };
    match env.pop() {
        Item::Bytes(b) => {
            let cache = config::PlatformConfig::get()?.cache;
            let hash = cache.put(&b, alias)?;
            env.push(hash);
            
            Ok(())
        }
        i => Err(Error::expected(fit!("bytes"), i)),
    }
}

/// Takes an inner environment from the top of the stack, and spawns a
/// tokio task to evaluate that environment.
pub fn animate(env: &mut Environment) -> StepResult {
    let tos = env.pop();
    let inner_env = Environment::try_derive(tos);
    match inner_env {
        Ok(inner) => {
            
            tokio::spawn(async move { eval(inner).await });
            StepResult::Done
        }
        Err(e) => {
            env.push(e);
            StepResult::Done
        }
    }
}

pub fn f_recur(env: &mut Environment) -> StepResult {
    let combinator = env.pop();
    let false_branch = env.pop();
    let true_branch = env.pop();
    let pred = env.pop();

    let combinator_list = coll::List::try_derive(combinator).unwrap_or_default();
    let false_branch_list = coll::List::try_derive(false_branch).unwrap_or_default();
    let true_branch_list = coll::List::try_derive(true_branch).unwrap_or_default();
    let pred_list = coll::List::try_derive(pred).unwrap_or_default();

    let chunk = crate::compile::compile_recur_with_dict(&pred_list, &true_branch_list, &false_branch_list, &combinator_list, Some(&env.dictionary));

    let mut prog = Program::default();
    prog.push_frame(crate::types::container::program::Frame {
        chunk: Arc::new(chunk),
        ip: 0,
        loop_counters: vec![],
    });
    
    env.push(Item::Program(Box::new(prog)));
    StepResult::Done
}

fn xor_(i: Bytes, j: Bytes) -> Bytes {
    let len = std::cmp::max(i.len(), j.len());
    let mut result = Vec::with_capacity(len);
    for (byte_i, byte_j) in i
        .iter()
        .chain(std::iter::repeat(&0).take(len - i.len()))
        .zip(j.iter().chain(std::iter::repeat(&0).take(len - j.len())))
    {
        result.push(byte_i ^ byte_j);
    }
    result
}
pub fn xor(i: Item, j: Item) -> ItemResult {
    match (i, j) {
        (Item::Int(i), Item::Int(j)) => Ok(Item::Int(i ^ j)),
        (Item::Bytes(i), Item::Bytes(j)) => Ok(xor_(*i, *j).fit()),
        (i, j) => Err(Error::expected(fit!("integers"), pair(i, j))),
    }
}

pub fn inspect(i: Item) -> String {
    format!("{:?}", i)
}

pub fn timestamps(env: &mut Environment) -> StepResult {
    
    env.push(Item::Time);
    StepResult::Done
}

pub fn standard(env: &mut Environment) -> StepResult {
    
    env.push(Item::Standard);
    StepResult::Done
}

pub fn intersection(i: Item, j: Item) -> ItemResult {
    let i = coll::Set::try_derive(i).map_err(Error::derive)?;
    let j = coll::Set::try_derive(j).map_err(Error::derive)?;
    let ij = i.intersection(&j);
    let h = std::collections::HashSet::from_iter(ij.cloned());
    Ok(coll::Set::derive(h).fit())
}

pub fn difference(i: Item, j: Item) -> ItemResult {
    let i = coll::Set::try_derive(i).map_err(Error::derive)?;
    let j = coll::Set::try_derive(j).map_err(Error::derive)?;
    let ij = i.difference(&j);
    let h = std::collections::HashSet::from_iter(ij.cloned());
    Ok(coll::Set::derive(h).fit())
}

pub fn compare(i: Item, j: Item) -> ItemResult {
    fn res(r: Option<std::cmp::Ordering>, i: Item, j: Item) -> ItemResult {
        match r {
            Some(std::cmp::Ordering::Less) => Ok(fit!("less")),
            Some(std::cmp::Ordering::Equal) => Ok(fit!("equal")),
            Some(std::cmp::Ordering::Greater) => Ok(fit!("greater")),
            None => Err(Error::expected(fit!("comparable"), pair(i, j))),
        }
    }
    match (i, j) {
        (Item::Float(fi), Item::Float(fj)) => {
            let r = fi.partial_cmp(&fj);
            res(r, fi.fit(), fj.fit())
        }
        (Item::Int(fi), Item::Float(fj)) => {
            let r = (fi as f64).partial_cmp(&fj);
            res(r, fi.fit(), fj.fit())
        }
        (Item::Float(fi), Item::Int(fj)) => {
            let r = fi.partial_cmp(&(fj as f64));
            res(r, fi.fit(), fj.fit())
        }
        (i, j) => {
            let ki = assoc::KeyItem::try_derive(i).map_err(Error::derive)?;
            let kj = assoc::KeyItem::try_derive(j).map_err(Error::derive)?;
            let r = ki.partial_cmp(&kj);
            res(r, ki.fit(), kj.fit())
        }
    }
}

fn as_pair(i: Item) -> Result<(Item, assoc::KeyItem), Error> {
    let mut i = coll::List::try_derive(i).map_err(Error::derive)?;
    let j = i.pop_front().ok_or(Error::short_list(1))?;
    let k = i
        .pop_front()
        .ok_or(Error::short_list(2))
        .and_then(|i| assoc::KeyItem::try_derive(i).map_err(Error::derive))?;
    Ok((j, k))
}

pub fn sort_by_key(l: coll::Sized) -> Result<coll::List, Error> {
    let it = l.into_iter().map(as_pair);
    let mut it = it.collect::<Result<Vec<(Item, assoc::KeyItem)>, Error>>()?;
    it.sort_unstable_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Less));
    Ok(it.into_iter().map(|(k, _)| k).rewrap())
}

fn slice(arr: coll::Sized, mut start: Int, mut end: Int) -> ItemResult {
    //println!("Start: {:?}, End: {:?}", start, end);
    //let arr = coll::Sized::try_derive(arr)?;
    //let mut start = Int::try_derive(start)?;
    //let mut end = Int::try_derive(end)?;

    if start < 0 {
        start += arr.count() as i64;
    }
    if end <= 0 && end != start {
        end += arr.count() as i64;
    }

    if start > end || start < 0 || end > arr.count() as i64 {
        return Err(Error::create(
            list!("range?"),
            "invalid index range",
            Some(pair(start.fit(), end.fit())),
        ));
    }
    match arr {
        coll::Sized::Bytes(arr) => Ok(arr
            .get(start as usize..end as usize)
            .map(|a| a.to_vec())
            .fit()),
        coll::Sized::String(arr) => Ok(arr
            .get(start as usize..end as usize)
            .map(|a| a.to_string())
            .fit()),
        coll::Sized::List(mut arr) => {
            let sliced = arr.slice(start as usize..end as usize);
            Ok(sliced.fit())
        }
        i => Err(Error::expected(fit!("ordered"), i)),
    }
}

fn empty(s: Item) -> Result<coll::Sized, Error> {
    let s = coll::Sized::try_derive(s)?;
    Ok(s.empty().fit())
}

fn format(items: Item, fstr: Item) -> ItemResult {
    let fstr = String::try_derive(fstr).map_err(Error::derive)?;
    let items = coll::List::try_derive(items).map_err(Error::derive)?;
    let vecitems: Vec<Item> = items.into_iter().collect();
    Ok(SimpleCurlyFormat
        .format(fstr.as_str(), &vecitems)?
        .into_owned()
        .fit())
}

impl From<dynfmt::Error<'_>> for Error {
    fn from(err: dynfmt::Error) -> Error {
        Error::create(list!("format"), &err.to_string(), Option::<Item>::None)
    }
}

impl From<Utf8Error> for Error {
    fn from(err: Utf8Error) -> Error {
        Error::create(list!("decode"), &err.to_string(), Option::<Item>::None)
    }
}

fn decode_json(s: String) -> ItemResult {
    Ok(serde_json::from_str::<Item>(s.as_str())?)
}

fn encode_json(i: Item) -> ItemResult {
    Ok(Item::derive(serde_json::to_string(&i)?))
}

fn namespace(word: Word, ns: Bytes) -> Word {
    let mut word = word.as_ref().clone();
    word.namespace = Some(Intern::new(ns));
    word.fit()
}

fn unnamespace(env: &mut Environment) -> StepResult {
    if let Some(i) = env.stack.pop_front() {
        match Word::try_derive(i) {
            Ok(w) => {
                let mut w = w.as_ref().clone();
                
                if let Some(ns) = w.namespace {
                    w.namespace = None;
                    env.push(w);
                    env.push(Item::derive(ns.as_ref().clone()));
                } else {
                    env.push(w);
                    env.push(Item::default())
                }
            }
            Err(e) => {
                env.push(e);
            }
        }
    } else {
        env.push(Error::stack_underflow())
    }
    StepResult::Done
}

fn resolve(env: &mut Environment) -> StepResult {
    if let Some(i) = env.stack.pop_front() {
        match Word::try_derive(i) {
            Ok(w) => {
                
                let mut w2 = w.as_ref().clone();
                if let Some(e) = env.dictionary.lingo.get(&w) {
                    w2.namespace = Some(e.namespace);
                }
                env.push(w2);
            }
            Err(e) => {
                env.push(e);
            }
        }
    } else {
        env.push(Error::stack_underflow())
    }

    StepResult::Done
}

fn is_finished(env: Environment) -> bool {
    env.is_finished()
}

fn stackpoint(w: &Word) -> Option<(usize, bool)> {
    let s = String::derive(*w);

    // Define mappings for number emojis
    const NUMBER_EMOJIS: [&str; 9] = ["1️⃣", "2️⃣", "3️⃣", "4️⃣", "5️⃣", "6️⃣", "7️⃣", "8️⃣", "9️⃣"];

    // Check for splice (scissors + number)
    if let Some(rest) = s.strip_prefix("✂️") {
        if let Some(position) = NUMBER_EMOJIS.iter().position(|&emoji| emoji == rest) {
            return Some((position + 1, true));
        }
    // Check for just the number (insert operation)
    } else if let Some(position) = NUMBER_EMOJIS.iter().position(|&emoji| emoji == s) {
        return Some((position + 1, false));
    }

    None
}

#[allow(dead_code)]
fn key(w: &Word) -> Option<(Word, bool)> {
    // Check for splice (scissors + tag + word)
    if let Some(rest) = w.strip_prefix("✂️") {
        rest.strip_prefix("🏷️").map(|k| (k, true))

        // Check for just the tag
    } else {
        w.strip_prefix("🏷️").map(|k| (k, false))
    }
}

fn pack(env: &mut Environment) -> Result<(), Error> {
    let template: coll::List = env.pop().try_fit().map_err(Error::derive)?;
    let mut last_stack_item_used = 0;

    /// Takes an accumulator that is a pair of the stack and the
    /// the item is a template placeholder, inserts or splices the
    /// item from the stack.
    fn splice(
        acc: Result<(coll::List, &mut usize, Vec<Item>), Error>,
        i: Item,
    ) -> Result<(coll::List, &mut usize, Vec<Item>), Error> {
        match acc {
            Ok((stack, last_stack_item_used, mut list)) => match i {
                Item::Word(w) => {
                    if let Some((n, is_splice)) = stackpoint(&w) {
                        *last_stack_item_used = max(n, *last_stack_item_used);
                        stack
                            .get(n - 1)
                            .ok_or_else(|| Error::list_count(n as Int))
                            .cloned()
                            .and_then(|item| {
                                if is_splice {
                                    let s = coll::Sized::try_derive(item).map_err(Error::derive)?;
                                    list.extend(s)
                                } else {
                                    list.push(item);
                                }
                                Ok((stack, last_stack_item_used, list))
                            })
                    } else {
                        list.push(Item::Word(w));
                        Ok((stack, last_stack_item_used, list))
                    }
                }
                Item::List(l) => {
                    // recurse
                    let (stack, last_stack_item_used, filled) = l.iter().cloned().fold(
                        Ok((stack.clone(), last_stack_item_used, Vec::new())),
                        splice,
                    )?;
                    list.push(coll::List::derive(filled).fit());
                    Ok((stack, last_stack_item_used, list))
                }
                i => {
                    list.push(i);
                    Ok((stack, last_stack_item_used, list))
                }
            },
            Err(e) => Err(e),
        }
    }

    let (_, last_stack_item_used, filled) = template.iter().cloned().fold(
        Ok((env.stack.to_list(), &mut last_stack_item_used, Vec::new())),
        splice,
    )?;
    
    // pop all the used items
    for _ in 0..*last_stack_item_used {
        env.pop();
    }
    env.push(coll::List::derive(filled));
    Ok(())
}

/// A struct to keep track of destructured values from the function
/// unpack. Can only have max 9 params because it's meant to go on the
/// stack - more than that and it's recommended to use named params
/// that get returned as an association.
#[derive(Debug, Clone, Default)]
struct PositionalParams<T> {
    values: [Option<T>; 9],
}

impl<T: Clone> PositionalParams<T>
where
    T: Fit<Item> + std::fmt::Debug,
{
    // Set a value at position (1-9)
    fn set(&mut self, position: usize, value: T) -> Result<(), Error> {
        //println!("Setting {} to {:?}", position, value);
        if !(1..=9).contains(&position) {
            return Err(Error::out_of_range(position as Int, 1, 10));
            //return Err(format!("Position must be between 1-9, got {}", position));
        }

        // Convert to 0-based index
        let idx = position - 1;

        // Check if already set
        if self.values[idx].is_some() {
            return Err(Error::create(
                list!("set-position"),
                "Position is already set",
                Some(position as Int),
            ));
        }

        self.values[idx] = Some(value);
        Ok(())
    }

    // Get a value at position (1-9)
    #[allow(dead_code)]
    fn get(&self, position: usize) -> Option<&T> {
        if !(1..=9).contains(&position) {
            return None;
        }
        self.values[position - 1].as_ref()
    }

    // Merge with another set of positional parameters
    fn merge(&mut self, other: &Self) -> Result<(), Error> {
        //let mut result = self.clone();

        for i in 0..9 {
            if let Some(value) = &other.values[i] {
                if self.values[i].is_some() {
                    return Err(Error::create(
                        list!("assign-param"),
                        "Pattern matching params can't be used more than once",
                        Some(value.clone()),
                    ));
                }
                self.values[i] = Some(value.clone());
            }
        }

        Ok(())
    }

    // Convert to a vector, useful for pushing onto stack
    fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        //println!("postional params {:?}", self);
        self.values.iter().filter_map(|opt| opt.clone()).collect()
    }
}

const TAG_EMOJI: &str = "🏷️";
const WILDCARD: &str = "_";

/// Destructuring (inverse of pack). Takes a Sized input item to
/// unpack, and a pattern. The pattern should contain stack markers
/// like 1️⃣. Anywhere those markers occur in the pattern, the
/// corresponding value will be extracted from the input and placed at
/// that location on the stack.
fn unpack(env: &mut Environment) -> Result<(), Error> {
    fn is_map_variable(w: &Word) -> bool {
        String::derive(*w).starts_with(TAG_EMOJI)
    }

    fn is_wildcard(w: &Word) -> bool {
        String::derive(*w) == WILDCARD
    }

    fn is_catch_all(w: &Word) -> bool {
        stackpoint(w).map(|(_, splice)| splice).unwrap_or(false)
    }

    fn strip_variable(w: &Word) -> Word {
        if let Some(stripped) = w.strip_prefix(TAG_EMOJI) {
            stripped.fit()
        } else {
            *w
        }
    }

    fn match_err<T, U>(a: T, b: U) -> Error
    where
        T: Fit<Item>,
        U: Fit<Item>,
    {
        Error::create(list![a, b, "="], "match failed", Option::<Item>::None)
    }

    fn destructure(
        data: &Item,
        pattern: &Item,
        mut acc_assoc: HashMap<Word, Item>,
        mut acc_seq: PositionalParams<Item>,
    ) -> Result<(HashMap<Word, Item>, PositionalParams<Item>), Error> {
        // see if both are maps
        //println!("Destructure: {:?} with {:?}", data, pattern);
        let data_sized: Result<coll::Sized, crate::types::container::error::Error> =
            data.clone().try_fit();
        let pattern_sized: Result<coll::Sized, crate::types::container::error::Error> =
            pattern.clone().try_fit();
        match (data_sized, pattern_sized) {
            (Ok(data), Ok(pattern)) => {
                // We got containers
                match (data, pattern) {
                    (coll::Sized::List(mut data_list), coll::Sized::List(pattern_list)) => {
                        let plen = pattern_list.len();
                        let dlen = data_list.len();

                        //println!("destructuring list: {:?}", data_list);
                        if pattern_list.len() != data_list.len() {
                            let is_catch = pattern_list
                                .get(plen - 1)
                                .and_then(|i| Word::try_derive(i).ok())
                                .map(|w| is_catch_all(&w))
                                .unwrap_or(false);

                            if !(is_catch && plen <= dlen + 1) {
                                // short pattern doesn't match without a catch-all
                                return Err(Error::create(list!("short-pattern"), "Pattern shorter than list can't match without a catch-all like ✂️1️⃣ at the end of the pattern.", Some(*pattern_list)));
                            }
                        }

                        let mut potential_updates_map: HashMap<Word, Item> = Default::default();
                        let mut potential_updates_list: PositionalParams<Item> = Default::default();
                        for i in 0..(pattern_list.len()) {
                            let p = pattern_list.get(i);
                            let v = data_list.get(i).cloned().unwrap_or_default();
                            match p {
                                Some(p) => {
                                    match p {
                                        Item::Word(w) => {
                                            if let Some((slot, is_catch)) = stackpoint(w) {
                                                //if last pattern item is catch_all
                                                if i == pattern_list.len() - 1 && is_catch {
                                                    // gather the rest of the data
                                                    let rest = data_list.slice(i..);
                                                    potential_updates_list.set(slot, rest.fit())?;
                                                } else if is_catch {
                                                    // catchall not allowed except at end.
                                                    return Err(
                                                        Error::create(list!("catch-all"), "Catch-all placeholder is only allowed at the end of the pattern.", Some(*pattern_list))
                                                       );
                                                } else {
                                                    potential_updates_list.set(slot, v)?;
                                                }
                                            } else if is_map_variable(w) {
                                                acc_assoc.insert(strip_variable(w), v);
                                            } else if is_wildcard(w) || Item::Word(*w) == v {
                                                // match! but not a variable so continue
                                            } else {
                                                // pattern does not match. bail out of list processing
                                                return Err(match_err(*w, v));
                                            }
                                        }
                                        p => {
                                            (potential_updates_map, potential_updates_list) =
                                                destructure(
                                                    &v,
                                                    p,
                                                    potential_updates_map,
                                                    potential_updates_list,
                                                )?
                                        }
                                    }
                                }
                                // past end of list
                                None => {
                                    break;
                                }
                            }
                        }
                        acc_assoc.extend(potential_updates_map);
                        acc_seq.merge(&potential_updates_list)?;
                        Ok((acc_assoc, acc_seq))
                    }

                    (a, b) => {
                        // see if it's associative
                        match (
                            assoc::Associative::try_derive(a.clone()),
                            assoc::Associative::try_derive(b.clone()),
                        ) {
                            (Ok(data), Ok(pattern)) => {
                                //println!("destructuring map: {:?}", data_map);
                                //map destructure
                                // Iterate over the entries
                                let mut potential_updates_map: HashMap<Word, Item> =
                                    Default::default();
                                let mut potential_updates_list: PositionalParams<Item> =
                                    Default::default();
                                for (k, ref v) in pattern.into_iter() {
                                    // look up k in data
                                    let vd = data.get(&k);
                                    // if v is a placeholder, vd is our target
                                    if let Some(vd) = vd {
                                        let r = <Word as crate::derivation::TryDerive<
                                            crate::types::Item,
                                        >>::try_derive(
                                            v.clone()
                                        );
                                        if let Ok(w) = r {
                                            if let Some((idx, _)) = stackpoint(&w) {
                                                let _ = potential_updates_list.set(idx, vd);
                                            } else if is_map_variable(&w) {
                                                potential_updates_map
                                                    .insert(strip_variable(&w), vd);
                                            } else {
                                                (potential_updates_map, potential_updates_list) =
                                                    destructure(
                                                        &vd,
                                                        v,
                                                        potential_updates_map,
                                                        potential_updates_list,
                                                    )?
                                            }
                                        } else {
                                            // value is not a word
                                            (potential_updates_map, potential_updates_list) =
                                                destructure(
                                                    &vd,
                                                    v,
                                                    potential_updates_map,
                                                    potential_updates_list,
                                                )?
                                        }
                                    } else {
                                        // key not present
                                        // if pattern value is placeholder, we need to return a []
                                        let r = Word::try_derive(v.clone());
                                        if let Ok(w) = r {
                                            if let Some((idx, _)) = stackpoint(&w) {
                                                let _ = potential_updates_list
                                                    .set(idx, Default::default());
                                            }
                                        }
                                    }
                                }
                                acc_assoc.extend(potential_updates_map);
                                let _ = acc_seq.merge(&potential_updates_list);
                                Ok((acc_assoc, acc_seq))
                            }
                            (Ok(aa), Err(_)) => Err(match_err(aa, b.clone())),
                            (Err(_), Ok(bb)) => Err(match_err(a.clone(), bb)),
                            (Err(_), Err(_)) => {
                                if a == b {
                                    Ok((acc_assoc, acc_seq))
                                } else {
                                    Err(match_err(a.clone(), b.clone()))
                                }
                            }
                        }
                    }
                }
            }
            (Err(_), Err(_)) => {
                let d = data.clone();
                let p = pattern.clone();
                if d == p
                    || Word::try_derive(p.clone())
                        .map(|w| is_wildcard(&w))
                        .unwrap_or_else(|_| false)
                {
                    Ok((acc_assoc, acc_seq))
                } else {
                    Err(match_err(p, d))
                }
            }
            (Ok(d), Err(_)) => {
                let d = Item::derive(d);
                if d == *pattern
                    || Word::try_derive(pattern.clone())
                        .map(|w| is_wildcard(&w))
                        .unwrap_or_else(|_| false)
                {
                    Ok((acc_assoc, acc_seq))
                } else {
                    Err(match_err(pattern.clone(), d))
                }
            }
            (Err(_), Ok(p)) => {
                let p = Item::derive(p);
                if p == *data {
                    Ok((acc_assoc, acc_seq))
                } else {
                    Err(match_err(p, data.clone()))
                }
            }
        }
    }
    let pattern = env.pop();
    let data = env.pop();
    let mut matches_map: HashMap<Word, Item> = Default::default();
    let mut matches_list: PositionalParams<Item> = Default::default();

    (matches_map, matches_list) = destructure(&data, &pattern, matches_map, matches_list)?;
    let res_map: assoc::AssociationContent = matches_map
        .into_iter()
        .map(|(k, v)| (assoc::KeyItem::derive(k), v))
        .collect();
    let res_list = matches_list.to_vec();
    //println!("Got unpack result list {:?}", res_list);
    
    env.push(coll::List::derive(res_list));
    env.push(Item::derive(res_map));
    Ok(())
}
