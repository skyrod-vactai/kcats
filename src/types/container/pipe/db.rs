use crate::axiom;
use crate::config;
use crate::derivation::*;
use crate::types::container::{self as coll, associative as assoc, error, error::Error};
use crate::types::{self, Item};
use crate::{fit, list};

use rusqlite::types::{ToSql, ToSqlOutput, Value, ValueRef};
use rusqlite::{params, Connection, Error as DBError};
use std::path::PathBuf;

use uuid;

pub struct Db {
    conn: Connection,
}

impl Db {
    pub fn new() -> Result<Self, DBError> {
        let db_file = config::PlatformConfig::get()
            .unwrap()
            .database
            .ok_or(DBError::InvalidPath(PathBuf::from("".to_string())))?;

        let conn = Connection::open(db_file.as_path())?;
        Ok(Db { conn })
    }

    pub fn query(&self, query: &str, params: Vec<(String, Item)>) -> axiom::ItemResult {
        let mut stmt = self.conn.prepare(query)?;

        // Convert Vec<Box<dyn ToSql>> to Vec<&dyn ToSql>
        let params_refs: Vec<(&str, &dyn ToSql)> = params
            .iter()
            .map(|(s, b)| (s.as_str(), b as &dyn ToSql))
            .collect();

        let rows = stmt.query_and_then(params_refs.as_slice(), |row| {
            (0..row.as_ref().column_count())
                .map(|column_index| {
                    let column_name = row.as_ref().column_name(column_index).unwrap().to_string();
                    let column_value: ValueRef = row.get_ref_unwrap(column_index);
                    Item::try_derive(column_value).and_then(|v| {
                        assoc::KeyItem::try_derive(column_name.as_str()).map(|w| (w, v))
                    })
                })
                .my_collect::<Result<assoc::Association, _>>()
                .map(Item::derive)
        })?;
        Ok(rows.my_collect::<Result<coll::List, _>>()?.fit())
    }

    fn insert_attribute(&self, id: uuid::Uuid, attribute: Item, value: Item) -> axiom::ItemResult {
        let q = "INSERT INTO EAV (entity, attribute, value) VALUES (?, ?, ?)";

        match value {
            //TODO maybe seamlessly support other Associative types
            Item::Assoc(a) => {
                let sub_id = uuid::Uuid::new_v4();
                self.insert_item(a.fit(), sub_id)?;
                self.conn.execute(q, params![id, attribute, sub_id])?;
            }
            Item::Set(s) => self.insert_iter(
                Some((id, attribute)),
                s.iter().map(|i| Item::derive(i.clone())),
            )?,
            Item::List(l) => self.insert_iter(Some((id, attribute)), *l)?,
            i => {
                self.conn.execute(q, params![id, attribute, i])?;
            }
        }
        Ok(Item::default())
    }

    pub fn insert_iter<I>(&self, parent_link: Option<(uuid::Uuid, Item)>, l: I) -> Result<(), Error>
    where
        I: IntoIterator<Item = Item>,
    {
        for v in l {
            if is_value(&v) {
                match parent_link {
                    Some(parent_link) => {
                        self.insert_attribute(parent_link.0, parent_link.1.clone(), v.clone())?;
                        return Ok(());
                    }
                    None => {
                        return Err(Error::expected(fit!("parent-link"), v.clone()));
                    }
                }
            } else {
                let sub_id = uuid::Uuid::new_v4();
                self.insert_item(v.clone(), sub_id)?;
                if let Some(parent_link) = parent_link.clone() {
                    self.insert_attribute(
                        parent_link.0,
                        parent_link.1.clone(),
                        sub_id.into_bytes().to_vec().fit(),
                    )?;
                }
            }
        }
        Ok(())
    }

    pub fn insert_item(&self, i: Item, id: uuid::Uuid) -> axiom::ItemResult {
        let s = coll::Sized::try_derive(i)?;
        match s {
            coll::Sized::List(l) => {
                self.insert_iter(None, *l)?;
            }
            s => {
                let a = assoc::Associative::try_derive(s)?;
                for (k, v) in a.to_iter() {
                    //println!("Insert! {:?} {:?}", k, v);
                    let w: types::Word = k.try_fit()?;
                    self.insert_attribute(id, Item::Word(w), v)?;
                }
            }
        }
        Ok(Item::default())
    }
}

pub fn is_value(i: &Item) -> bool {
    matches!(
        i,
        Item::String(_)
            | Item::Bytes(_)
            | Item::Int(_)
            | Item::Float(_)
            | Item::Word(_)
            | Item::Char(_)
    )
}

pub fn query(q: Item, params: Item) -> axiom::ItemResult {
    let query: String = q.try_fit()?;
    // Needs to be association, and instead of a slice of ToSql,
    // we need &[(&str, &dyn ToSql)] (slice of pairs of strings and ToSql)
    let params: assoc::Associative = params.try_fit()?;

    let mut boxed_params: Vec<(String, Item)> = Vec::new();
    for (k, v) in params.to_iter() {
        match k {
            assoc::KeyItem::String(s) => boxed_params.push((s, v)),
            k => return Err(Error::expected(fit!("string"), k)),
        }
    }

    let db = Db::new()?;
    db.query(&query, boxed_params)
}

pub fn insert_object(i: Item) -> axiom::ItemResult {
    let db = Db::new()?;
    let id = uuid::Uuid::new_v4();
    db.insert_item(i, id)
}

impl TryDerive<ValueRef<'_>> for Item {
    fn try_derive(value: ValueRef) -> Result<Self, Error> {
        match value {
            ValueRef::Integer(i) => Ok(Item::Int(i)),
            ValueRef::Real(f) => Ok(Item::Float(f)),
            ValueRef::Text(t) => decode_string(String::from_utf8_lossy(t).into_owned()),

            ValueRef::Blob(b) => Ok(Item::Bytes(Box::new(b.to_vec()))),
            ValueRef::Null => Ok(Item::default()),
        }
    }
}
/// Since sqlite doesn't have separate string/word/char types, we
/// store them all as String, and encode a prefix to note which type
/// it should be when decoded.
fn decode_string(s: String) -> Result<Item, Error> {
    if let Some(w) = s.strip_prefix("w|") {
        w.try_fit()
    } else if let Some(s) = s.strip_prefix("s|") {
        Ok(Item::String(Box::new(s.to_string())))
    } else if let Some(c) = s.strip_prefix("c|") {
        let char_seq = c;
        if char_seq.chars().count() == 1 {
            Ok(Item::Char(char_seq.chars().next().unwrap()))
        } else {
            Err(Error::expected(fit!("char"), char_seq.to_string()))
        }
    } else {
        Err(Error::expected(fit!("string"), s))
    }
}

enum EncodeAs {
    String(String),
    Char(types::Char),
    Word(types::Word),
}

impl EncodeAs {
    fn encode(self: EncodeAs) -> ToSqlOutput<'static> {
        ToSqlOutput::Owned(Value::Text(match self {
            EncodeAs::String(s) => format!("s|{}", s),
            EncodeAs::Word(w) => format!("w|{}", String::derive(w)),
            EncodeAs::Char(c) => format!("c|{}", String::from(c)),
        }))
    }
}

impl rusqlite::ToSql for Item {
    fn to_sql(&self) -> Result<ToSqlOutput<'_>, DBError> {
        match self {
            Item::Int(i) => i.to_sql(),
            Item::Float(f) => f.to_sql(),
            Item::Char(c) => Ok(EncodeAs::Char(*c).encode()),
            Item::Word(w) => Ok(EncodeAs::Word(w.clone()).encode()),
            Item::String(s) => Ok(EncodeAs::String((**s).clone()).encode()),
            Item::Bytes(b) => b.to_sql(),
            _ => todo!("convert item variants to sql values"),
        }
    }
}

impl From<rusqlite::Error> for Error {
    fn from(error: rusqlite::Error) -> Self {
        Error::create(
            list!("io"),
            error.to_string().as_str(),
            Option::<Item>::None,
        )
    }
}

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Constraint {
    pub entity: Item,
    pub attribute: Item,
    pub value: Item,
}

impl TryDerive<coll::List> for Constraint {
    fn try_derive(mut l: coll::List) -> Result<Constraint, Error> {
        if l.len() != 3 {
            Err(error::Error::list_count(3))
        } else {
            let entity: Item = l.pop_front().unwrap().try_fit()?;
            let attribute: Item = l.pop_front().unwrap().try_fit()?;
            let value: Item = l.pop_front().unwrap().try_fit()?;
            Ok(Constraint {
                entity,
                attribute,
                value,
            })
        }
    }
}

fn is_var(v: &Item) -> bool {
    if let Item::Word(s) = v {
        s.data.ends_with('?')
    } else {
        false
    }
}

fn var_name(s: &str) -> &str {
    s.trim_end_matches('?')
}

/// Returns (SQL String, Parameters Map)
pub fn datalog_to_sql(
    constraints: Vec<Constraint>,
    selection: Vec<Item>,
) -> Result<(String, HashMap<String, Item>), Error> {
    if constraints.is_empty() {
        return Ok(("SELECT 1".to_string(), HashMap::new()));
    }

    // 1. Setup
    let mut var_map: HashMap<types::Word, Vec<(usize, String)>> = HashMap::new();
    let mut conditions: Vec<Vec<String>> = vec![Vec::new(); constraints.len()];
    let mut params: HashMap<String, Item> = HashMap::new();

    for (idx, c) in constraints.iter().enumerate() {
        let alias = format!("c{}", idx);

        // --- Process Entity ---
        if is_var(&c.entity) {
            var_map
                .entry(c.entity.clone().try_fit().unwrap())
                .or_default()
                .push((idx, "Entity".to_string()));
        } else {
            // It's a literal: Create placeholder and store value
            let p_name = format!(":{}entity", alias); // e.g., :c0entity
            conditions[idx].push(format!("{}.Entity = {}", alias, p_name));
            params.insert(p_name, c.entity.clone());
        }

        // --- Process Attribute ---
        if is_var(&c.attribute) {
            var_map
                .entry(c.attribute.clone().try_fit().unwrap())
                .or_default()
                .push((idx, "Attribute".to_string()));
        } else {
            let p_name = format!(":{}attribute", alias); // e.g., :c0attribute
            conditions[idx].push(format!("{}.Attribute = {}", alias, p_name));
            params.insert(p_name, c.attribute.clone());
        }

        // --- Process Value ---
        if is_var(&c.value) {
            var_map
                .entry(c.value.clone().try_fit().unwrap())
                .or_default()
                .push((idx, "Value".to_string()));
        } else {
            let p_name = format!(":{}value", alias); // e.g., :c0value
            conditions[idx].push(format!("{}.Value = {}", alias, p_name));
            params.insert(p_name, c.value.clone());
        }
    }

    // 2. Generate Linkage (Join) Conditions based on variables
    // (Logic remains the same as before, no params needed for joins)
    for (_, occurrences) in var_map.iter() {
        if occurrences.len() > 1 {
            let (first_tbl_idx, first_col) = &occurrences[0];

            for (curr_tbl_idx, curr_col) in occurrences.iter().skip(1) {
                let cond = format!(
                    "c{}.{} = c{}.{}",
                    curr_tbl_idx, curr_col, first_tbl_idx, first_col
                );
                conditions[*curr_tbl_idx].push(cond);
            }
        }
    }

    // 3. Build SELECT Clause
    let select_parts: Vec<String> = selection
        .iter()
        .cloned()
        .map(|var| {
            let r = types::Word::try_derive(var);
            match r {
                Ok(w) => Ok(if let Some(locs) = var_map.get(&w) {
                    let (tbl_idx, col) = &locs[0];
                    format!("c{}.{} AS {}", tbl_idx, col, var_name(w.data.as_str()))
                } else {
                    format!("NULL AS {}", var_name(w.data.as_str()))
                }),
                Err(e) => Err(e),
            }
        })
        .collect::<Result<Vec<String>, Error>>()?;

    let select_clause = format!("SELECT {}", select_parts.join(", "));

    // 4. Build FROM and JOIN Clauses
    let mut query = select_clause;
    query.push_str("\nFROM EAV as c0");

    for i in 1..constraints.len() {
        let alias = format!("c{}", i);
        let on_clause = if conditions[i].is_empty() {
            "1=1".to_string()
        } else {
            conditions[i].join(" AND ")
        };
        query.push_str(&format!("\nJOIN EAV {} ON {}", alias, on_clause));
    }

    // 5. Build WHERE Clause
    if !conditions[0].is_empty() {
        query.push_str(&format!("\nWHERE {}", conditions[0].join(" AND ")));
    }

    Ok((query, params))
}

// pub fn build_query(mut env: Environment) -> Result<(), Error> {
//     let selection = coll::List::try_derive(env.pop())?;
//     let constraintslist = coll::List::try_derive(env.pop())?;
//     let constraints: Vec<Constraint> = constraintslist.try_fit()?;
//     let (sql, params) = datalog_to_sql(constraints, selection.fit())?;
// }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{fit, list};
    use std::collections::HashMap;

    #[test]
    fn test_simple() {
        let constraints: Vec<Constraint> = vec![
            list!["book?", "subjects", "love"].try_fit().unwrap(),
            list!["book?", "title", "title?"].try_fit().unwrap(),
            list!["book?", "author-last", "author-last?"]
                .try_fit()
                .unwrap(),
        ];
        let selection: Vec<Item> =
            vec![Item::Word(fit!("author-last?")), Item::Word(fit!("title?"))];
        let (sql, params) = datalog_to_sql(constraints, selection).unwrap();
        assert_eq!(sql, "SELECT c2.Value AS author-last, c1.Value AS title\nFROM EAV as c0\nJOIN EAV c1 ON c1.Attribute = :c1attribute AND c1.Entity = c0.Entity\nJOIN EAV c2 ON c2.Attribute = :c2attribute AND c2.Entity = c0.Entity\nWHERE c0.Attribute = :c0attribute AND c0.Value = :c0value");
        assert_eq!(
            params,
            HashMap::from([
                (":c0attribute".to_string(), Item::Word(fit!("subjects"))),
                (":c0value".to_string(), Item::Word(fit!("love"))),
                (":c1attribute".to_string(), Item::Word(fit!("title"))),
                (":c2attribute".to_string(), Item::Word(fit!("author-last"))),
            ]),
        )
    }
}
