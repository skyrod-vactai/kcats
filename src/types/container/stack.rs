
use crate::types::container::List;
use crate::types::Item;

#[derive(Clone, Default, Debug)]
pub struct StackData {
    pub tos: Option<Item>,
    pub nos: Option<Item>,
    pub nnd: Option<Item>,
    pub rest: List,
}

pub type Stack = StackData;

impl StackData {

    pub fn shuffle(&mut self, pops: u8, pushes: &[u8]) -> Result<(), crate::types::Error> {
        if pops == 0 && pushes.is_empty() {
            return Ok(());
        }
        
        let mut items = Vec::with_capacity(pops as usize);
        for _ in 0..pops {
            if let Some(item) = self.pop_front() {
                items.push(item);
            } else {
                for i in (0..items.len()).rev() {
                    self.push_front(items[i].clone());
                }
                return Err(crate::types::Error::stack_underflow());
            }
        }
        
        for &idx in pushes {
            self.push_front(items[idx as usize].clone());
        }

        Ok(())
    }

    pub fn push_front(&mut self, item: Item) {
        if self.tos.is_none() {
            self.tos = Some(item);
        } else if self.nos.is_none() {
            self.nos = self.tos.take();
            self.tos = Some(item);
        } else if self.nnd.is_none() {
            self.nnd = self.nos.take();
            self.nos = self.tos.take();
            self.tos = Some(item);
        } else {
            self.rest.push_front(self.nnd.take().unwrap());
            self.nnd = self.nos.take();
            self.nos = self.tos.take();
            self.tos = Some(item);
        }
    }

    pub fn pop_front(&mut self) -> Option<Item> {
        if let Some(tos) = self.tos.take() {
            self.tos = self.nos.take();
            self.nos = self.nnd.take();
            if !self.rest.is_empty() {
                self.nnd = Some(self.rest.pop_front().unwrap());
            }
            Some(tos)
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.tos.is_some() as usize
            + self.nos.is_some() as usize
            + self.nnd.is_some() as usize
            + self.rest.len()
    }

    pub fn is_empty(&self) -> bool {
        self.tos.is_none() && self.rest.is_empty()
    }

    pub fn front(&self) -> Option<&Item> {
        self.tos.as_ref()
    }

    pub fn to_list(&self) -> List {
        let mut list = self.rest.clone();
        if let Some(nnd) = &self.nnd {
            list.push_front(nnd.clone());
        }
        if let Some(nos) = &self.nos {
            list.push_front(nos.clone());
        }
        if let Some(tos) = &self.tos {
            list.push_front(tos.clone());
        }
        list
    }

    pub fn get(&self, index: usize) -> Option<&Item> {
        let cached =
            self.tos.is_some() as usize + self.nos.is_some() as usize + self.nnd.is_some() as usize;
        if index < cached {
            match index {
                0 => self.tos.as_ref(),
                1 => self.nos.as_ref(),
                2 => self.nnd.as_ref(),
                _ => unreachable!(),
            }
        } else {
            self.rest.get(index - cached)
        }
    }

    pub fn front_mut(&mut self) -> Option<&mut Item> {
        if let Some(tos) = self.tos.as_mut() {
            Some(tos)
        } else {
            self.rest.front_mut()
        }
    }

    pub fn from_list(mut list: List) -> Self {
        let tos = list.pop_front();
        let nos = list.pop_front();
        let nnd = list.pop_front();
        Self {
            tos,
            nos,
            nnd,
            rest: list,
        }
    }
}

use crate::derivation::{Derive, TryDerive};

impl Derive<Stack> for List {
    fn derive(s: Stack) -> List {
        s.to_list()
    }
}

impl Derive<List> for Stack {
    fn derive(l: List) -> Stack {
        StackData::from_list(l)
    }
}

impl Derive<Stack> for Item {
    fn derive(s: Stack) -> Item {
        Item::List(Box::new(s.to_list()))
    }
}

impl TryDerive<Item> for Stack {
    fn try_derive(i: Item) -> Result<Stack, crate::types::container::error::Error> {
        let l = List::try_derive(i)?;
        Ok(StackData::from_list(l))
    }
}

impl StackData {
    pub fn replace1(&mut self, item: Item) {
        self.tos = Some(item);
    }

    pub fn replace2(&mut self, item: Item) {
        self.tos = Some(item);
        self.nos = self.nnd.take();
        if !self.rest.is_empty() {
            self.nnd = Some(self.rest.pop_front().unwrap());
        }
    }

    pub fn replace3(&mut self, item: Item) {
        self.tos = Some(item);
        self.nos = if !self.rest.is_empty() {
            Some(self.rest.pop_front().unwrap())
        } else {
            None
        };
        self.nnd = if !self.rest.is_empty() {
            Some(self.rest.pop_front().unwrap())
        } else {
            None
        };
    }

    pub fn swap(&mut self, a: usize, b: usize) {
        if a == b {
            return;
        }

        let max_idx = std::cmp::max(a, b);
        let min_idx = std::cmp::min(a, b);
        let cached = self.tos.is_some() as usize + self.nos.is_some() as usize
            + self.nnd.is_some() as usize;

        if max_idx < cached {
            if min_idx == 0 && max_idx == 1 {
                std::mem::swap(&mut self.tos, &mut self.nos);
            } else if min_idx == 0 && max_idx == 2 {
                std::mem::swap(&mut self.tos, &mut self.nnd);
            } else if min_idx == 1 && max_idx == 2 {
                std::mem::swap(&mut self.nos, &mut self.nnd);
            }
            return;
        }

        // Fallback for deeper swaps
        let mut list = self.to_list();
        list.swap(a, b);
        *self = StackData::from_list(list);
    }
}

impl PartialEq for StackData {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for i in 0..self.len() {
            if self.get(i) != other.get(i) {
                return false;
            }
        }
        true
    }
}
