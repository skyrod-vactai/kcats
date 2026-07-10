use crate::derivation::*;
use crate::types::container as cont;
use crate::types::container::Error;
use crate::types::container::SimpleTake;
use crate::types::Item;
use crate::types::Word;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Push(Item),
    Call(Word),
    Execute(Arc<Chunk>),
    Dip(Arc<Chunk>),
    Jump(isize),
    JumpIfTrue(isize),
    JumpIfFalse(isize),
    JumpIfFalseKeepIfTrue(isize),
    
    // Loop counters
    IncLoopCounter(usize),
    DecLoopCounter(usize),
    PushLoopCounter,
    PopLoopCounter,
    JumpIfLoopCounterZero(usize, isize),
    
    Return,
    Shuffle { pops: u8, pushes: Vec<u8> },
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Chunk {
    pub ops: Vec<Op>,
    pub source: Option<cont::List>,
}

#[derive(Clone, Debug)]
pub struct Frame {
    pub chunk: Arc<Chunk>,
    pub ip: usize,
    pub loop_counters: Vec<usize>,
    pub restore_items: Vec<crate::types::Item>,
}

impl PartialEq for Frame {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.chunk, &other.chunk) && self.ip == other.ip
    }
}

impl Frame {
    pub fn is_finished(&self) -> bool {
        self.ip >= self.chunk.ops.len()
    }

    #[inline(always)]
    pub fn next_op(&mut self) -> Option<Op> {
        if self.is_finished() {
            None
        } else {
            let op = self.chunk.ops[self.ip].clone();
            self.ip += 1;
            Some(op)
        }
    }
}

#[derive(Clone, PartialEq, Default, Debug)]
pub struct Program(pub Vec<Frame>);

impl Program {
    pub fn clean(&mut self) -> Vec<crate::types::Item> {
        let mut restored = Vec::new();
        while self.0.last().map(|f| f.is_finished()).unwrap_or(false) {
            if let Some(frame) = self.0.pop() {
                for item in frame.restore_items.into_iter().rev() {
                    restored.push(item);
                }
            }
        }
        restored
    }
    
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    
    pub fn prepend(&mut self, snippet: cont::List) {
        if !snippet.is_empty() {
            let chunk = crate::compile::compile(&snippet);
            self.0.push(Frame {
                chunk: Arc::new(chunk),
                ip: 0,
                loop_counters: vec![],
                restore_items: vec![],
            });
        }
    }
    
    pub fn prepend_program(&mut self, mut program: Program) {
        program.clean();
        self.0.extend(program.0);
    }
    
    pub fn push_frame(&mut self, frame: Frame) {
        self.0.push(frame);
    }
    
    pub fn pop_frame(&mut self) -> Option<Frame> {
        self.0.pop()
    }
    
    pub fn extend_program(&mut self, other: Program) {
        self.0.extend(other.0);
    }
    
    pub fn extend(&mut self, snippet: cont::List) {
        if !snippet.is_empty() {
            let chunk = crate::compile::compile(&snippet);
            self.0.insert(
                0,
                Frame {
                    chunk: Arc::new(chunk),
                    ip: 0,
                    loop_counters: vec![],
                    restore_items: vec![],
                },
            )
        }
    }

    pub fn unwind_to_handle(&mut self, handle: &Item) -> (Program, bool) {
        let mut unwound = Program::default();
        let mut found = false;

        while let Some(f) = self.0.last_mut() {
            if f.is_finished() {
                self.0.pop();
                continue;
            }

            let mut local_found = false;
            let start_idx = f.ip;
            while f.ip < f.chunk.ops.len() {
                let op = &f.chunk.ops[f.ip];
                f.ip += 1;
                match op {
                    Op::Call(w) if &Item::Word(*w) == handle => { local_found = true; break; }
                    Op::Push(i) if i == handle => { local_found = true; break; }
                    _ => {}
                }
            }
            if local_found {
                found = true;
                let end_idx = f.ip - 1;
                let unwound_ops = f.chunk.ops[start_idx..end_idx].to_vec();
                let unwound_frame = Frame {
                    chunk: Arc::new(Chunk { ops: unwound_ops, source: None }),
                    ip: 0,
                    loop_counters: f.loop_counters.clone(),
                    restore_items: vec![],
                };
                unwound.0.push(unwound_frame);
                break;
            } else {
                let mut unwound_frame = f.clone();
                unwound_frame.ip = start_idx;
                unwound.0.push(unwound_frame);
                self.0.pop();
            }
        }
        unwound.0.reverse();
        (unwound, found)
    }

    pub fn count(&self) -> usize {
        self.0.iter().fold(0, |acc, f| acc + (f.chunk.ops.len().saturating_sub(f.ip)))
    }
    
    pub fn stacktrace(&self) -> cont::List {
        // Try to construct a stacktrace based on recent calls or ops
        let mut s = cont::List::new();
        for f in self.0.iter().rev() {
            if f.ip > 0 && f.ip <= f.chunk.ops.len() {
                let last_op = &f.chunk.ops[f.ip - 1];
                if let Op::Call(w) = last_op {
                    s.push_back(Item::Word(*w));
                }
            }
        }
        s
    }
}

impl Derive<cont::List> for Program {
    fn derive(s: cont::List) -> Program {
        Program(vec![Frame {
            chunk: Arc::new(crate::compile::compile(&s)),
            ip: 0,
            loop_counters: vec![],
            restore_items: vec![],
        }])
    }
}

impl Derive<Program> for cont::List {
    fn derive(mut p: Program) -> cont::List {
        let mut r = cont::List::new();
        for f in p.0.iter_mut().rev() {
            r.append(crate::compile::decompile(&f.chunk))
        }
        r
    }
}

impl Derive<Program> for Item {
    fn derive(p: Program) -> Item {
        Item::derive(cont::List::derive(p))
    }
}

impl TryDerive<Item> for Program {
    fn try_derive(i: Item) -> Result<Program, Error> {
        let s: cont::Sized = i.try_fit()?;
        match s {
            cont::Sized::Program(p) => Ok(p),
            s => Ok(Program::derive(cont::List::try_derive(s)?)),
        }
    }
}

impl SimpleTake for Program {
    type Item = Item;
    fn take_simple(&mut self) -> Option<Self::Item> {
        // Can't easily take an item from a program now.
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::list;

    #[test]
    fn test_layers() {
        let p = Program(vec![
            Frame {
                chunk: Arc::new(crate::compile::compile(&list!["foo", "bar", "baz"])),
                ip: 1,
                loop_counters: vec![],
                restore_items: vec![],
            },
        ]);
        assert_eq!(p.count(), 3);
    }
}
