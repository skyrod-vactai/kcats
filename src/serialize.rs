//! Serializes and parses kcats data. kcats serialization is inspired
//! by Joy, and implemented as a subset of edn (where only vector
//! containers from edn are used, no lists, maps or sets). Currently
//! one custom tag is used for encoding byte arrays, but this is
//! subject to change.
use crate::derivation::*;
use crate::fit;
use crate::traits::*;
use crate::types::container::{
    self as coll, associative as assoc, environment::Environment, error::Error, pipe::standard,
    pipe::time,
};
use crate::types::*;

use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use edn_format::ParserOptions;
use futures::executor;

use internment::Intern;
use std::fmt;
use std::string;

pub trait Display {
    fn representation(&self) -> Item;
}

const BYTE_TAG: &str = "b64";

/// Parses a serialized value into an [Item].
impl TryDerive<edn_format::Value> for Item {
    fn try_derive(item: edn_format::Value) -> Result<Item, Error> {
        //println!("to item {:?}", item);
        match item {
            edn_format::Value::Integer(i) => Ok(Item::Int(i)),
            // the problem here is the function level doesn't know if
            // there's an inner conversion being done or not. It
            // assumes not, but here there is one. We need some
            // polymorphism here. For now we just clear the inner.
            edn_format::Value::Vector(v) => Ok(v.try_rewrap::<coll::List>().fit()),
            edn_format::Value::Symbol(s) => Ok(Item::Word(Intern::new(WordData {
                data: Intern::new(s.name().to_string()),
                namespace: s
                    .namespace()
                    .map(|ns| BASE64_URL_SAFE_NO_PAD.decode(ns))
                    .transpose()?
                    .map(Intern::new),
                quoted: false,
            }))),
            // we don't have booleans in kcats, so if we see 'false' that
            // is the word false which is not defined in the base
            // language, but might be user-defined later.
            edn_format::Value::Boolean(b) => Ok(if b { fit!("yes") } else { fit!("false") }),
            edn_format::Value::String(s) => Ok(s.to_string().fit()),
            edn_format::Value::Float(f) => Ok(Item::Float(f.into_inner())),
            edn_format::Value::TaggedElement(tag, e) => {
                if tag == edn_format::Symbol::from_name(BYTE_TAG) {
                    if let edn_format::Value::String(s) = *e {
                        Ok(BASE64_URL_SAFE_NO_PAD
                            .decode(s.clone().into_bytes())
                            .unwrap()
                            .fit())
                    } else {
                        Err(Error::parse("Invalid tag datatype for byte literal"))
                    }
                } else {
                    Err(Error::parse("Unsupported tag"))
                }
            }
            edn_format::Value::Character(c) => Ok(Item::Char(c)),
            _ => Err(Error::parse("Unsupported data literal")),
        }
    }
}

/// Serializes the item deterministically. Certain data is lost in
/// serialization, including the type of container (sets/maps/lists
/// all are serialized as vectors)
impl From<&Item> for edn_format::Value {
    fn from(item: &Item) -> Self {
        fn from_assoc(a: assoc::Associative) -> edn_format::Value {
            let mut av = a
                .clone()
                .into_iter()
                .collect::<Vec<(assoc::KeyItem, Item)>>();
            av.sort_by(|(ka, _), (kb, _)| ka.cmp(kb));
            edn_format::Value::Vector(
                av.into_iter()
                    .map(|i| (&Item::derive(i)).into())
                    .collect::<Vec<edn_format::Value>>(),
            )
        }

        match item {
            // dictionaries are big and it's ugly to print them for
            // environments.
            Item::Int(i) => edn_format::Value::Integer(*i),
            Item::Float(f) => edn_format::Value::from(*f),
            Item::Char(c) => edn_format::Value::Character(*c),
            Item::Builtin(b) => edn_format::Value::Symbol(edn_format::Symbol::from_name(
                format!("builtin_{}", b.name).as_str(),
            )),
            Item::Word(w) => edn_format::Value::Symbol({
                if let Some(ns) = w.namespace {
                    edn_format::Symbol::from_namespace_and_name(
                        BASE64_URL_SAFE_NO_PAD.encode(ns.as_ref()).as_str(),
                        w.fit(),
                    )
                } else {
                    edn_format::Symbol::from_name(w.fit())
                }
            }),
            //Item::Entry(w) => edn_format::Value::Symbol(edn_format::Symbol::from_name(&w.word)),
            Item::Standard => edn_format::Value::from(&standard::representation()),
            Item::StaticFile(f) => edn_format::Value::from(&executor::block_on(async move {
                f.read().await.representation()
            })),
            Item::ServerSocket(f) => edn_format::Value::from(&executor::block_on(async move {
                f.read().await.representation()
            })),
            Item::Socket(f) => edn_format::Value::from(&executor::block_on(async move {
                f.read().await.representation()
            })),
            Item::Handoff(t) => (&t.representation()).into(),
            Item::Timer(t) => edn_format::Value::from(&t.representation()),
            Item::Time => edn_format::Value::from(&time::representation()),
            Item::Words(e) => edn_format::Value::Symbol(edn_format::Symbol::from_name(
                format!("{}_entries", e.len()).as_str(),
            )),
            Item::Env(e) => (&e.representation()).into(),
            Item::String(s) => edn_format::Value::String(s.to_string()),
            Item::Bytes(bs) => edn_format::Value::TaggedElement(
                edn_format::Symbol::from_name("b64"),
                Box::new(edn_format::Value::String(
                    BASE64_URL_SAFE_NO_PAD.encode(&**bs),
                )),
            ),
            Item::Assoc(a) => from_assoc(assoc::Associative::Assoc(a.clone())),
            Item::DictEntry(a) => from_assoc(assoc::Associative::DictEntry(a.clone())),

            Item::Error(a) => from_assoc(assoc::Associative::Error(a.clone())),

            Item::Dictionary(a) => from_assoc(assoc::Associative::Dictionary(a.clone())),
            Item::Nothing => edn_format::Value::Vector(vec![]),
            Item::Set(s) => {
                let mut v = s.iter().cloned().collect::<Vec<assoc::KeyItem>>();
                v.sort();
                edn_format::Value::Vector(
                    v.into_iter().map(|ki| (&Item::derive(ki)).into()).collect(),
                )
            }
            Item::List(v) => edn_format::Value::Vector(
                v.clone()
                    .into_iter()
                    .map(|i| (&i).into())
                    .collect::<Vec<edn_format::Value>>(),
            ),
            Item::Program(v) => edn_format::Value::Vector(
                //TODO not efficient, maybe make an iterator for program
                coll::List::derive((**v).clone())
                    .into_iter()
                    .map(|i| (&i).into())
                    .collect::<Vec<edn_format::Value>>(),
            ),
        }
    }
}

impl TryDerive<Result<edn_format::Value, crate::types::container::error::Error>> for Item {
    fn try_derive(
        r: Result<edn_format::Value, crate::types::container::error::Error>,
    ) -> Result<Item, Error> {
        match r {
            Ok(val) => match val.clone().try_fit() {
                Ok(val) => Ok(val),
                Err(pe) => Err(Error::parse(format!("{:?}", pe).as_str())), //TODO: problem is edn_format structs are not items, maybe allow them to be convertible?
            },
            Err(e) => Err(e),
        }
    }
}

pub fn parse(s: String) -> Result<coll::List, Error> {
    let parser = edn_format::Parser::from_iter(s.chars(), edn_format::ParserOptions::default());
    parser
        .into_iter()
        .map(|x| x.unwrap().try_fit())
        .collect::<Result<crate::types::container::List, _>>()
}

/// A streaming parser function that takes the state of the parsing
/// off the stack and uses the edn-format crate to do the heavy
/// lifting. Reads all the remaining objects and returns a list of
/// them plus whatever string is leftover (empty string if nothing
/// left)
/// Note: This implementation is primarily designed for parsing
/// complete EDN data structures (lists, vectors, maps, etc).
/// When parsing raw tokens (symbols, numbers, etc) at chunk
/// boundaries, it may occasionally fail to properly join tokens
/// that span multiple chunks.
pub fn parse_edn(env: &mut Environment) -> Result<(), Error> {
    let mut obj_buffer: coll::List = env.pop().try_fit().map_err(Error::derive)?;
    let next_input: String = env.pop().try_fit().map_err(Error::derive)?;
    let mut state: String = env.pop().try_fit().map_err(Error::derive)?;

    state.push_str(next_input.as_str());

    let mut parser = edn_format::Parser {
        opts: ParserOptions::default(),
        iter: state.chars().peekable(),
    };

    // Collect items until we hit the end or can't parse anymore

    loop {
        // Save iterator state before attempting parse
        let before_parse = parser.iter.clone();

        match parser.next() {
            Some(Ok(v)) => {
                obj_buffer.push_back(v.clone().try_fit()?);
            }
            Some(Err(edn_format::ParserError::UnexpectedEndOfInput)) => {
                // Restore iterator to state before failed parse
                parser.iter = before_parse;
                break;
            }
            Some(Err(e)) => return Err(Error::from(e)),
            None => break,
        }
    }

    let remaining: String = parser.iter.collect();
    env.push(Item::derive(remaining));
    env.push(Item::derive(obj_buffer));
    Ok(())
}

/// A function to use with a streaming parser, converting from byte
/// arrays to strings. Returns whatever couldn't be parsed from the
/// byte array, in the event a utf8 character is incomplete.
fn split_at_utf8_boundary(bytes: &[u8]) -> (String, Vec<u8>) {
    match std::str::from_utf8(bytes) {
        Ok(s) => (s.to_string(), Vec::new()),
        Err(e) => {
            // e.valid_up_to() tells us the position where valid UTF-8 ends
            let valid_str = String::from_utf8_lossy(&bytes[..e.valid_up_to()]).into_owned();
            let remainder = bytes[e.valid_up_to()..].to_vec();
            (valid_str, remainder)
        }
    }
}

pub fn parse_utf8(env: &mut Environment) -> Result<(), Error> {
    let buffer = env.pop();
    let next_input: Bytes = env.pop().try_fit().map_err(Error::derive)?;
    let mut state: Bytes = env.pop().try_fit().map_err(Error::derive)?;

    state.extend(next_input);

    let mut obj_buffer: coll::List = Default::default();
    if !buffer.is_empty() {
        obj_buffer = buffer.try_fit().map_err(Error::derive)?;
    }
    let (parsed, remaining) = split_at_utf8_boundary(&state);

    obj_buffer.push_back(parsed.fit());
    env.push(Item::derive(remaining));
    env.push(Item::derive(obj_buffer));
    Ok(())
}

pub trait Emit {
    fn emit(self) -> String;
}

impl Emit for &Item {
    fn emit(self) -> String {
        edn_format::emit_str(&(self).into())
    }
}

impl<I, T> Emit for I
where
    I: Iterator<Item = T>,
    T: Emit,
{
    fn emit(self) -> String {
        let mut s: String = String::new();
        for i in self {
            s.push_str(i.emit().as_str());
            s.push(' ');
        }
        s.pop();
        s.to_string()
    }
}

// print out envs in error messages
impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{ stack: {}, program: {} }}",
            (&Item::derive(self.stack.to_list())).emit(),
            (&Item::derive(self.program.clone())).emit(),
        )
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", (&Item::derive(self.data.clone())).emit())
    }
}

impl From<edn_format::ParserError> for Error {
    fn from(e: edn_format::ParserError) -> Self {
        let s = match e {
            edn_format::ParserError::UnexpectedCharacter(c) => {
                format!("Unexpected Character: {}", c)
            }
            e => e.to_string(),
        };
        Error::parse(s.as_str())
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(e: string::FromUtf8Error) -> Self {
        Error::parse(e.to_string().as_str())
    }
}

impl From<base64::DecodeError> for Error {
    fn from(e: base64::DecodeError) -> Self {
        Error::parse(e.to_string().as_str())
    }
}

fn insert_line_breaks(input: &str, max_items: usize, max_chars: usize) -> String {
    let mut result = String::new();
    let mut current_line_length = 0;
    let mut open_list_stack: Vec<(usize, usize)> = Vec::new();
    open_list_stack.push((0, 0));
    let mut last_char: char = '\n';
    let mut in_string: bool = false;
    let mut in_tag = false;
    let mut chars = input.chars().peekable(); // Convert to a Peekable iterator

    while let Some(c) = chars.next() {
        current_line_length += 1;

        match c {
            '"' => {
                if last_char != '\\' {
                    in_string = !in_string;
                }
                result.push(c);
            }
            '[' => {
                if !in_string && last_char != '\\' {
                    open_list_stack.push((0, 0)); // Start a new list
                }
                result.push(c);
            }
            ']' => {
                result.push(c);
                if !in_string && last_char != '\\' {
                    let (last_count, break_count) = open_list_stack.pop().unwrap();
                    //println!("items, breaks: {}, {}", last_count, break_count);
                    if (last_count == 1 || last_count >= 6 || break_count > 0)
                        && chars.peek() != Some(&']')
                    {
                        // Only add a newline if the next character is not a closing bracket
                        result.push('\n');
                        let (_, break_count) = open_list_stack.last_mut().unwrap();
                        *break_count += 1;
                        current_line_length = 0;
                    }
                }
            }
            ' ' => {
                if !in_string {
                    let (last_count, break_count) = open_list_stack.last_mut().unwrap();
                    if in_tag {
                        in_tag = false;
                    } else {
                        *last_count += 1;
                    }
                    if (*last_count > 0 && (*last_count % max_items) == 0)
                        || current_line_length > max_chars
                    {
                        result.push('\n');
                        *break_count += 1;
                        current_line_length = 0;
                        //*last_count = 0;
                    }
                }
                result.push(c);
            }
            '#' => {
                if !in_string {
                    in_tag = true;
                }
                result.push(c);
            }
            _ => {
                result.push(c);
            }
        }
        last_char = c;
    }
    if result.ends_with('\n') {
        result.pop();
    }
    //println!("broken output: {:?}", result);
    result
}

fn parse_indent(stack: &mut Vec<usize>, input: &str) {
    let mut in_string = false;
    let mut escaped = false;

    for (idx, c) in input.chars().enumerate() {
        if in_string {
            match c {
                '"' if !escaped => in_string = false,
                // TODO handle \\ (escaped backslash char)
                '\\' if !escaped => escaped = true,
                _ => escaped = false,
            }
        } else {
            match c {
                '[' if !escaped => {
                    escaped = false;
                    stack.push(idx);
                }
                ']' if !escaped => {
                    escaped = false;
                    stack.pop();
                }
                '"' => {
                    escaped = false;
                    in_string = true;
                }
                ';' => {
                    break;
                }
                '\\' => {
                    escaped = true;
                }
                _ => {
                    escaped = false;
                }
            }
        }
    }
}

fn format_indentation(input: &str) -> String {
    let mut result = String::new();
    let mut indentations = Vec::<usize>::new();

    for line in input.lines() {
        let trimmed = line.trim();

        // Deduce the new indentation based on the last item in the indentations stack
        let new_indent = indentations.last().copied().map(|x| x + 1).unwrap_or(0);
        let padded_line = format!("{}{}\n", " ".repeat(new_indent), trimmed);
        result.push_str(padded_line.as_str());
        parse_indent(&mut indentations, &padded_line);
        //println!("indentations: {:?}: {:?}", padded_line, indentations);
    }
    result.pop(); // Remove the last newline
    result
}

pub fn auto_format(input: &str, max_items: usize, max_chars: usize) -> String {
    let with_breaks = insert_line_breaks(input, max_items, max_chars);
    format_indentation(&with_breaks)
}

/// a function that takes an env, and an input string. Parses the
/// string, if it parses, returns the env with the input added to the
/// program. Otherwise returns Error.
pub fn parse_input(env: &mut Environment, input: String) -> Result<(), Error> {
    let parsed = parse(input)?;
    env.program.extend(parsed);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_line_breaks() {
        let input = "[[foo bar][baz [[quux floop][toop poop]]]]";
        let expected = "[[foo bar]\n[baz [[quux floop]\n[toop poop]]]]";
        let output = insert_line_breaks(input, 6, 80);
        assert_eq!(output, expected);

        let input = "[[[1 2 3] b][c d]]";
        let expected = "[[[1 2 3] b]\n[c d]]";
        let output = insert_line_breaks(input, 6, 80);
        assert_eq!(output, expected);

        // multiline list
        let input = "[[a b] [c d]] 5";
        let expected = "[[a b]\n [c d]]\n 5";
        let output = insert_line_breaks(input, 6, 80);
        assert_eq!(output, expected);
    }

    #[test]
    fn test_indentation() {
        let input = "[[foo bar]\n[baz [[quux floop]\n[toop poop]]]]";
        let expected = "[[foo bar]\n [baz [[quux floop]\n       [toop poop]]]]";
        let output = format_indentation(input);
        assert_eq!(output, expected);

        let input = "\"hello\" [[a b]\n[c d]]";
        let expected = "\"hello\" [[a b]\n         [c d]]";
        let output = format_indentation(input);
        assert_eq!(output, expected);
    }
}
