use crate::types::container::program::{Chunk, Op};
use crate::types::container as cont;
use crate::types::Item;
use crate::types::container::dictionary::Dictionary;
use crate::types::container::dictionary::Executable;

struct VirtualStack {
    items: Vec<usize>,
    pops: usize,
}

impl VirtualStack {
    fn new() -> Self {
        Self { items: Vec::new(), pops: 0 }
    }

    fn pop(&mut self) -> usize {
        if let Some(i) = self.items.pop() {
            i
        } else {
            let i = self.pops;
            self.pops += 1;
            i
        }
    }

    fn push(&mut self, i: usize) {
        self.items.push(i);
    }
    
    fn apply_shuffle(&mut self, pops: usize, pushes: &[u8]) {
        let mut popped = Vec::with_capacity(pops);
        for _ in 0..pops {
            popped.push(self.pop());
        }
        for &idx in pushes.iter().rev() {
            self.push(popped[idx as usize]);
        }
    }

    fn flush(&mut self, ops: &mut Vec<Op>) {
        if self.pops == 0 && self.items.is_empty() {
            return;
        }
        // Simplification pass
        while !self.items.is_empty() && self.pops > 0 {
            if self.items[0] == self.pops - 1 {
                let mut used_again = false;
                for i in 1..self.items.len() {
                    if self.items[i] == self.pops - 1 {
                        used_again = true;
                        break;
                    }
                }
                if used_again {
                    break;
                }
                self.items.remove(0);
                self.pops -= 1;
            } else {
                break;
            }
        }
        if self.pops > 0 || !self.items.is_empty() {
            let mut pushes = Vec::with_capacity(self.items.len());
            for &item in &self.items {
                // item is the original depth (0 = tos).
                // it needs to fit into a u8.
                pushes.push(item as u8);
            }
            ops.push(Op::Shuffle {
                pops: self.pops as u8,
                pushes,
            });
        }
        self.items.clear();
        self.pops = 0;
    }
}

fn chunk_to_shuffle(chunk: &Chunk) -> Option<(u8, Vec<u8>)> {
    let mut shuffle = None;
    for op in &chunk.ops {
        match op {
            Op::Shuffle { pops, pushes } => {
                if shuffle.is_none() {
                    let mut tos_to_deepest = Vec::with_capacity(pushes.len());
                    for &idx in pushes.iter().rev() {
                        tos_to_deepest.push(idx);
                    }
                    shuffle = Some((*pops, tos_to_deepest));
                } else {
                    return None;
                }
            }
            Op::Return => break,
            _ => return None,
        }
    }
    Some(shuffle.unwrap_or_else(|| (0, Vec::new())))
}

pub fn compile(list: &cont::List) -> Chunk {
    compile_with_dict(list, None)
}

pub fn compile_with_dict(list: &cont::List, dict: Option<&Dictionary>) -> Chunk {
    let mut ops = Vec::new();
    let mut vs = VirtualStack::new();

    for item in list.iter() {
        match item {
            Item::Word(w) => {
                let w_str = w.data.as_str();
                
                // Stack twiddle primitives
                let mut handled_twiddle = false;
                match w_str {
                    "🗑️" | "drop" => { vs.pop(); handled_twiddle = true; }
                    "•🗑️" | "drop-down" => { vs.apply_shuffle(2, &[0]); handled_twiddle = true; }
                    "••🗑️" | "drop-deep" => { vs.apply_shuffle(3, &[0, 1]); handled_twiddle = true; }
                    "👥" | "dup" => { vs.apply_shuffle(1, &[0, 0]); handled_twiddle = true; }
                    "•👥" | "dup-down" => { vs.apply_shuffle(2, &[0, 1, 1]); handled_twiddle = true; }
                    "••👥" | "dup-deep" => { vs.apply_shuffle(3, &[0, 1, 2, 2]); handled_twiddle = true; }
                    "🔀" | "swap" => { vs.apply_shuffle(2, &[1, 0]); handled_twiddle = true; }
                    "•🔀" | "swap-down" => { vs.apply_shuffle(3, &[0, 2, 1]); handled_twiddle = true; }
                    "••🔀" | "swap-deep" => { vs.apply_shuffle(4, &[0, 1, 3, 2]); handled_twiddle = true; }
                    "⚓" | "sink" => { vs.apply_shuffle(3, &[1, 2, 0]); handled_twiddle = true; }
                    "•⚓" | "sink-down" => { vs.apply_shuffle(4, &[0, 2, 3, 1]); handled_twiddle = true; }
                    "••⚓" | "sink-deep" => { vs.apply_shuffle(5, &[0, 1, 3, 4, 2]); handled_twiddle = true; }
                    "🛟" | "float" => { vs.apply_shuffle(3, &[2, 0, 1]); handled_twiddle = true; }
                    "•🛟" | "float-down" => { vs.apply_shuffle(4, &[0, 3, 1, 2]); handled_twiddle = true; }
                    "••🛟" | "float-deep" => { vs.apply_shuffle(5, &[0, 1, 4, 2, 3]); handled_twiddle = true; }
                    _ => {}
                }
                
                if handled_twiddle {
                    continue;
                }

                // Check for pure shuffle inlining if dict is available
                if let Some(d) = dict {
                    if let Some(entry) = d.get_entry(w) {
                        if let Executable::Derived(ref chunk) = entry.definition {
                            if let Some((pops, pushes)) = chunk_to_shuffle(chunk) {
                                vs.apply_shuffle(pops as usize, &pushes);
                                continue;
                            }
                        }
                    }
                }
                vs.flush(&mut ops);
                
                if w_str == "▶️" {
                    if let Some(Op::Push(Item::List(l))) = ops.last() {
                        let l_clone = l.clone();
                        ops.pop();
                        let mut chunk = compile_with_dict(&l_clone, dict);
                        chunk.ops.pop(); // remove Return
                        ops.extend(chunk.ops);
                        continue;
                    }
                } else if w_str == "🪄" {
                    if let Some(Op::Push(Item::List(l))) = ops.last() {
                        let l_clone = l.clone();
                        ops.pop();
                        let chunk = compile_with_dict(&l_clone, dict);
                        if let Some((pops, pushes)) = chunk_to_shuffle(&chunk) {
                            let mut new_pushes = vec![0];
                            for &idx in &pushes {
                                new_pushes.push(idx + 1);
                            }
                            vs.apply_shuffle(pops as usize + 1, &new_pushes);
                            continue;
                        }
                        ops.push(Op::Dip(std::sync::Arc::new(chunk)));
                        continue;
                    }
                } else if w_str == "•🪄" {
                    if let Some(Op::Push(Item::List(l))) = ops.last() {
                        let l_clone = l.clone();
                        ops.pop();
                        let inner_chunk = compile_with_dict(&l_clone, dict);
                        if let Some((pops, pushes)) = chunk_to_shuffle(&inner_chunk) {
                            let mut new_pushes = vec![0, 1];
                            for &idx in &pushes {
                                new_pushes.push(idx + 2);
                            }
                            vs.apply_shuffle(pops as usize + 2, &new_pushes);
                            continue;
                        }
                        let dip_chunk = Chunk { ops: vec![Op::Dip(std::sync::Arc::new(inner_chunk)), Op::Return], source: None };
                        ops.push(Op::Dip(std::sync::Arc::new(dip_chunk)));
                        continue;
                    }
                } else if w_str == "••🪄" {
                    if let Some(Op::Push(Item::List(l))) = ops.last() {
                        let l_clone = l.clone();
                        ops.pop();
                        let inner_chunk = compile_with_dict(&l_clone, dict);
                        if let Some((pops, pushes)) = chunk_to_shuffle(&inner_chunk) {
                            let mut new_pushes = vec![0, 1, 2];
                            for &idx in &pushes {
                                new_pushes.push(idx + 3);
                            }
                            vs.apply_shuffle(pops as usize + 3, &new_pushes);
                            continue;
                        }
                        let dip_chunk1 = Chunk { ops: vec![Op::Dip(std::sync::Arc::new(inner_chunk)), Op::Return], source: None };
                        let dip_chunk2 = Chunk { ops: vec![Op::Dip(std::sync::Arc::new(dip_chunk1)), Op::Return], source: None };
                        ops.push(Op::Dip(std::sync::Arc::new(dip_chunk2)));
                        continue;
                    }
                } else if w_str == "↔️" {
                    let len = ops.len();
                    if len >= 2 {
                        if let (Op::Push(Item::List(f)), Op::Push(Item::List(t))) = (&ops[len-1], &ops[len-2]) {
                            let f_clone = f.clone();
                            let t_clone = t.clone();
                            ops.pop();
                            ops.pop();
                            
                            let jump_base_idx = ops.len();
                            ops.push(Op::JumpIfFalseKeepIfTrue(0)); // placeholder
                            
                            let mut true_chunk = compile_with_dict(&t_clone, dict);
                            true_chunk.ops.pop();
                            ops.extend(true_chunk.ops);
                            
                            let jump_start_idx = ops.len();
                            ops.push(Op::Jump(0)); // placeholder
                            
                            let base_case_idx = ops.len();
                            ops[jump_base_idx] = Op::JumpIfFalseKeepIfTrue((base_case_idx as isize) - (jump_base_idx as isize) - 1);
                            
                            let mut false_chunk = compile_with_dict(&f_clone, dict);
                            false_chunk.ops.pop();
                            ops.extend(false_chunk.ops);
                            
                            let end_idx = ops.len();
                            ops[jump_start_idx] = Op::Jump((end_idx as isize) - (jump_start_idx as isize) - 1);
                            
                            continue;
                        }
                    }
                }
                
                ops.push(Op::Call(w.clone()));
            }
            _ => {
                vs.flush(&mut ops);
                ops.push(Op::Push(item.clone()));
            }
        }
    }
    vs.flush(&mut ops);
    ops.push(Op::Return);
    Chunk { ops, source: Some(list.clone()) }
}

pub fn decompile(chunk: &Chunk) -> cont::List {
    if let Some(ref source) = chunk.source {
        return source.clone();
    }
    let mut list = cont::List::new();
    for op in &chunk.ops {
        match op {
            Op::Push(item) => list.push_back(item.clone()),
            Op::Call(w) => list.push_back(Item::Word(w.clone())),
            _ => {}
        }
    }
    list
}

pub fn compile_recur(
    pred: &cont::List,
    true_branch: &cont::List,
    false_branch: &cont::List,
    combinator: &cont::List,
) -> Chunk {
    compile_recur_with_dict(pred, true_branch, false_branch, combinator, None)
}

pub fn compile_recur_with_dict(
    pred: &cont::List,
    true_branch: &cont::List,
    false_branch: &cont::List,
    combinator: &cont::List,
    dict: Option<&Dictionary>
) -> Chunk {
    let mut is_execute_first = false;
    let mut comb_rest = combinator.clone();
    if let Some(Item::Word(w)) = comb_rest.front() {
        if w.as_ref().data.as_str() == "▶️" {
            is_execute_first = true;
            comb_rest.pop_front();
        }
    }

    let mut ops = Vec::new();
    
    // Non-TCO (push program back to stack)
    if !is_execute_first {
        let mut pred_chunk = compile_with_dict(pred, dict);
        pred_chunk.ops.pop();
        ops.extend(pred_chunk.ops);
        
        let jump_base_idx = ops.len();
        ops.push(Op::JumpIfFalseKeepIfTrue(0)); // placeholder
        
        let mut true_chunk = compile_with_dict(true_branch, dict);
        true_chunk.ops.pop();
        ops.extend(true_chunk.ops);
        
        let mut list = cont::List::new();
        list.push_back(crate::types::Item::List(Box::new(pred.clone())));
        list.push_back(crate::types::Item::List(Box::new(true_branch.clone())));
        list.push_back(crate::types::Item::List(Box::new(false_branch.clone())));
        list.push_back(crate::types::Item::List(Box::new(combinator.clone())));
        list.push_back(crate::fit!("🪆"));
        list.push_back(crate::fit!("▶️"));
        let mut prog = crate::types::container::program::Program::default();
        prog.prepend(list);
        ops.push(Op::Push(Item::Program(Box::new(prog))));
        
        let mut comb_chunk = compile_with_dict(combinator, dict);
        comb_chunk.ops.pop();
        ops.extend(comb_chunk.ops);
        
        ops.push(Op::Return);
        
        let base_case_idx = ops.len();
        ops[jump_base_idx] = Op::JumpIfFalseKeepIfTrue((base_case_idx as isize) - (jump_base_idx as isize) - 1);
        
        let mut then_chunk = compile_with_dict(false_branch, dict);
        then_chunk.ops.pop();
        ops.extend(then_chunk.ops);
        ops.push(Op::Return);
        
        return Chunk { ops, source: None };
    }

// TCO or Flattened Loop
    let mut comb_chunk = compile_with_dict(&comb_rest, dict);
    comb_chunk.ops.pop(); // remove Return
    let has_combinator = !comb_chunk.ops.is_empty();

    if has_combinator {
        ops.push(Op::PushLoopCounter);
    }
    let loop_start = ops.len();

    let mut pred_chunk = compile_with_dict(pred, dict);
    pred_chunk.ops.pop(); // remove Return
    ops.extend(pred_chunk.ops);

    let jump_base_idx = ops.len();
    ops.push(Op::JumpIfFalseKeepIfTrue(0)); // placeholder

    let mut true_chunk = compile_with_dict(true_branch, dict);
    true_chunk.ops.pop();
    ops.extend(true_chunk.ops);

    if has_combinator {
        ops.push(Op::IncLoopCounter(0)); // index 0 for current loop
    }
    let jump_start_offset = loop_start as isize - (ops.len() as isize + 1);
    ops.push(Op::Jump(jump_start_offset));

    let base_case_idx = ops.len();
    ops[jump_base_idx] = Op::JumpIfFalseKeepIfTrue((base_case_idx as isize) - (jump_base_idx as isize) - 1);

    let mut then_chunk = compile_with_dict(false_branch, dict);
    then_chunk.ops.pop();
    ops.extend(then_chunk.ops);

    if has_combinator {
        let unwind_start = ops.len();
        let jump_unwind_end_idx = ops.len();
        ops.push(Op::JumpIfLoopCounterZero(0, 0)); // placeholder

        ops.extend(comb_chunk.ops);

        ops.push(Op::DecLoopCounter(0));
        let jump_unwind_start_offset = unwind_start as isize - (ops.len() as isize + 1);
        ops.push(Op::Jump(jump_unwind_start_offset));

        let unwind_end_idx = ops.len();
        ops[jump_unwind_end_idx] = Op::JumpIfLoopCounterZero(0, (unwind_end_idx as isize) - (jump_unwind_end_idx as isize) - 1);

        ops.push(Op::PopLoopCounter);
    }
    ops.push(Op::Return);
    Chunk { ops, source: None }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::container::program::{Chunk, Op};
    use crate::types::Item;
    use crate::types::container::List;
    use internment::Intern;
    use crate::types::WordData;

    fn check_compile(word: &str, expected_pops: u8, expected_pushes: &[u8]) {
        let mut list = List::new();
        list.push_back(Item::Word(Intern::new(WordData {
            data: Intern::new(word.to_string()),
            quoted: false,
            namespace: None,
        })));
        let chunk = compile(&list);
        if expected_pops == 0 && expected_pushes.is_empty() {
            assert_eq!(chunk.ops.len(), 1, "Expected no shuffle for {}", word);
            assert!(matches!(chunk.ops[0], Op::Return));
            return;
        }
        assert_eq!(chunk.ops.len(), 2, "Expected 2 ops for {}", word);
        if let Op::Shuffle { pops, pushes } = &chunk.ops[0] {
            assert_eq!(*pops, expected_pops, "pops mismatch for {}", word);
            assert_eq!(pushes.as_slice(), expected_pushes, "pushes mismatch for {}", word);
        } else {
            panic!("Expected Shuffle for {}, got {:?}", word, chunk.ops[0]);
        }
    }

    #[test]
    fn test_shuffles() {
        check_compile("drop", 1, &[]);
        check_compile("swap", 2, &[0, 1]);
        check_compile("dup", 1, &[0, 0]);
        check_compile("sink", 3, &[0, 2, 1]);

        check_compile("swap-down", 3, &[1, 2, 0]);
        check_compile("sink-down", 4, &[1, 3, 2, 0]);
        
        check_compile("float", 3, &[1, 0, 2]);
        check_compile("float-down", 4, &[2, 1, 3, 0]);
    }
}
