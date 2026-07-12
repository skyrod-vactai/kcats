use crate::types::container::List;
use crate::types::Item;
use std::collections::VecDeque;

#[derive(Clone, Default, Debug)]
pub struct StackData {
    pub cache: VecDeque<Item>,
    pub base: List,
    pub base_pops: usize,
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

    pub fn new() -> Self {
        Self {
            cache: VecDeque::new(),
            base: List::new(),
            base_pops: 0,
        }
    }

    pub fn to_list(&self) -> List {
        let mut list = self.base.clone();
        if self.base_pops > 0 {
            list = list.split_off(self.base_pops);
        }
        if !self.cache.is_empty() {
            let mut cache_vec = List::new();
            for item in self.cache.iter().rev() {
                cache_vec.push_back(item.clone());
            }
            cache_vec.append(list);
            list = cache_vec;
        }
        list
    }

    pub fn from_list(list: List) -> Self {
        Self {
            cache: VecDeque::new(),
            base: list,
            base_pops: 0,
        }
    }

    pub fn push_front(&mut self, item: Item) {
        self.cache.push_back(item);
    }

    pub fn pop_front(&mut self) -> Option<Item> {
        if let Some(item) = self.cache.pop_back() {
            Some(item)
        } else if self.base_pops < self.base.len() {
            let item = self.base[self.base_pops].clone();
            self.base_pops += 1;
            Some(item)
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.cache.len() + (self.base.len().saturating_sub(self.base_pops))
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn clear(&mut self) {
        self.cache.clear();
        self.base = List::new();
        self.base_pops = 0;
    }

    pub fn get(&self, index: usize) -> Option<&Item> {
        if index < self.cache.len() {
            self.cache.get(self.cache.len() - 1 - index)
        } else {
            let base_idx = index - self.cache.len() + self.base_pops;
            if base_idx < self.base.len() {
                Some(&self.base[base_idx])
            } else {
                None
            }
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut Item> {
        if index < self.cache.len() {
            let cache_len = self.cache.len();
            self.cache.get_mut(cache_len - 1 - index)
        } else {
            let base_idx = index - self.cache.len() + self.base_pops;
            if base_idx < self.base.len() {
                Some(self.base.get_mut(base_idx).unwrap())
            } else {
                None
            }
        }
    }

    pub fn front(&self) -> Option<&Item> {
        self.get(0)
    }

    pub fn front_mut(&mut self) -> Option<&mut Item> {
        self.get_mut(0)
    }

    pub fn replace1(&mut self, item: Item) {
        if !self.cache.is_empty() {
            let last = self.cache.len() - 1;
            self.cache[last] = item;
        } else if self.base_pops < self.base.len() {
            self.base[self.base_pops] = item;
        }
    }

    pub fn replace2(&mut self, item: Item) {
        if self.len() > 1 {
            *self.get_mut(1).unwrap() = item;
            self.pop_front();
        }
    }

    pub fn replace3(&mut self, item: Item) {
        if self.len() > 2 {
            *self.get_mut(2).unwrap() = item;
            self.pop_front();
            self.pop_front();
        }
    }

    pub fn swap(&mut self, a: usize, b: usize) {
        if a >= self.len() || b >= self.len() || a == b {
            return;
        }
        let item_a = self.get(a).unwrap().clone();
        let item_b = self.get(b).unwrap().clone();
        *self.get_mut(a).unwrap() = item_b;
        *self.get_mut(b).unwrap() = item_a;
    }

    pub fn iter(&self) -> impl Iterator<Item = &Item> {
        self.cache
            .iter()
            .rev()
            .chain(self.base.iter().skip(self.base_pops))
    }
}

impl crate::derivation::Derive<List> for StackData {
    fn derive(value: List) -> Self {
        StackData::from_list(value)
    }
}

impl crate::derivation::Derive<StackData> for List {
    fn derive(value: StackData) -> Self {
        value.to_list()
    }
}

impl crate::derivation::Derive<StackData> for Item {
    fn derive(value: StackData) -> Self {
        Item::List(Box::new(value.to_list()))
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
