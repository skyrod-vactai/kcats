use crate::types::container as cont;
use crate::types::container::dictionary::Dictionary;
use crate::types::container::dictionary::Executable;
use crate::types::container::program::{Chunk, Op};
use crate::types::Item;

/// `VirtualStack` simulates stack manipulations at compile-time to collapse
/// adjacent stack twiddles (like `swap`, `dup`, `drop`) into a single `Op::Shuffle` instruction.
///
/// It maintains a virtual state of the stack relative to the actual runtime stack at the start of a basic block.
/// The `items` vector represents the current stack ordering natively: `items[0]` is the Top-of-Stack (ToS),
/// `items[1]` is Next-on-Stack (NoS), and so on down to the deepest tracked element.
struct VirtualStack {
    items: Vec<usize>,
    pops: usize,
}

impl VirtualStack {
    /// Creates a fresh, empty VirtualStack state.
    fn new() -> Self {
        Self {
            items: Vec::new(),
            pops: 0,
        }
    }

    /// Pops the Top-of-Stack from the virtual state.
    /// If the virtual stack is currently empty, it means we are popping a value
    /// that existed on the runtime stack *before* this block of twiddles started.
    /// We represent this original depth using `self.pops`.
    fn pop(&mut self) -> usize {
        if self.items.is_empty() {
            let i = self.pops;
            self.pops += 1;
            i
        } else {
            self.items.remove(0)
        }
    }

    /// Pushes an item onto the Top-of-Stack of the virtual state.
    /// Since index 0 is the ToS, we insert the item at the front of the vector.
    fn push(&mut self, item: usize) {
        self.items.insert(0, item);
    }

    /// Applies a compile-time shuffle to the virtual stack state.
    ///
    /// `pushes` is expected to be an array of indices referencing the items just popped,
    /// ordered from Top-of-Stack to Deepest. For example, a `swap` operation takes `&[1, 0]`.
    fn apply_shuffle(&mut self, pops: usize, pushes: &[u8]) {
        let mut popped = Vec::with_capacity(pops);
        for _ in 0..pops {
            popped.push(self.pop());
        }
        for &idx in pushes.iter().rev() {
            self.push(popped[idx as usize]);
        }
    }
    /// Flushes the current virtual state, emitting an optimized `Op::Shuffle` instruction
    /// into the bytecode output `ops` if any stack mutations actually occurred.
    fn flush(&mut self, ops: &mut Vec<Op>) {
        if self.pops == 0 && self.items.is_empty() {
            return;
        }
        // Simplification pass: We trim redundant items from the bottom of the virtual stack.
        // If the deepest pushed item (which lives at the end of the `items` vector) maps directly
        // to the deepest popped original item (`self.pops - 1`), and it is not duplicated anywhere else,
        // we can safely trim it out. This prevents emitting `Op::Shuffle` instructions that just
        // pop and push the exact same item back where it started.
        while !self.items.is_empty() && self.pops > 0 {
            if *self.items.last().unwrap() == self.pops - 1 {
                let mut used_again = false;
                for i in 0..self.items.len() - 1 {
                    if self.items[i] == self.pops - 1 {
                        used_again = true;
                        break;
                    }
                }
                if used_again {
                    break;
                }
                self.items.pop();
                self.pops -= 1;
            } else {
                break;
            }
        }
        if self.pops > 0 || !self.items.is_empty() {
            let mut pushes = Vec::with_capacity(self.items.len());
            // Note: `Op::Shuffle` executes at runtime by popping values and then reading the `pushes`
            // array. It expects the array to be ordered from DEEPEST-to-TOS. Since our internal `items`
            // is naturally ordered TOS-to-DEEPEST, we must explicitly reverse it here at the boundary.
            for &item in self.items.iter().rev() {
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

/// A helper function to identify if a given `Chunk` consists exclusively of a single pure stack shuffle.
/// This allows us to transparently inline complex composite twiddles (like derived `swapdown` words)
/// or optimize the inner blocks of `dip` combinators into flat bytecode.
fn chunk_to_shuffle(chunk: &Chunk) -> Option<(u8, Vec<u8>)> {
    let mut temp_vs = VirtualStack::new();
    for op in &chunk.ops {
        match op {
            Op::Shuffle { pops, pushes } => {
                let mut rev_pushes = Vec::with_capacity(pushes.len());
                for &idx in pushes.iter().rev() {
                    rev_pushes.push(idx);
                }
                temp_vs.apply_shuffle(*pops as usize, &rev_pushes);
            }
            Op::Return => break,
            _ => return None,
        }
    }
    let mut dummy_ops = Vec::new();
    temp_vs.flush(&mut dummy_ops);
    if dummy_ops.is_empty() {
        Some((0, Vec::new()))
    } else if let Op::Shuffle { pops, pushes } = &dummy_ops[0] {
        let mut tos_to_deepest = Vec::with_capacity(pushes.len());
        for &idx in pushes.iter().rev() {
            tos_to_deepest.push(idx);
        }
        Some((*pops, tos_to_deepest))
    } else {
        None
    }
}

/// Main entry point for compiling a `List` of items into executable bytecode (`Chunk`).

/// Compiles a list, optionally using a local dictionary to perform aggressive compile-time inlining.
/// This handles iterating through the AST, matching builtin words, applying peephole stack optimizations,
/// and generating combinator branches (like `evaluate`, `dip`, `↔️`).
pub fn compile_with_dict(list: &cont::List, dict: &Dictionary) -> Chunk {
    let mut ops = Vec::new();
    let mut vs = VirtualStack::new();

    for item in list.iter() {
        match item {
            Item::Word(w) => {
                let w_str = w.data.as_str();

                let is_overridden = {
                    if let Some(entry) = dict.get_entry(w) {
                        matches!(entry.definition, Executable::Derived(_))
                    } else {
                        false
                    }
                };

                // Stack twiddle primitives
                let mut handled_twiddle = false;
                if !is_overridden {
                    match w_str {
                        "🗑️" | "drop" => {
                            vs.pop();
                            handled_twiddle = true;
                        }
                        "•🗑️" | "drop-down" => {
                            vs.apply_shuffle(2, &[0]);
                            handled_twiddle = true;
                        }
                        "••🗑️" | "drop-deep" => {
                            vs.apply_shuffle(3, &[0, 1]);
                            handled_twiddle = true;
                        }
                        "👥" | "dup" => {
                            vs.apply_shuffle(1, &[0, 0]);
                            handled_twiddle = true;
                        }
                        "•👥" | "dup-down" => {
                            vs.apply_shuffle(2, &[0, 1, 1]);
                            handled_twiddle = true;
                        }
                        "••👥" | "dup-deep" => {
                            vs.apply_shuffle(3, &[0, 1, 2, 2]);
                            handled_twiddle = true;
                        }
                        "🔀" | "swap" => {
                            vs.apply_shuffle(2, &[1, 0]);
                            handled_twiddle = true;
                        }
                        "•🔀" | "swap-down" => {
                            vs.apply_shuffle(3, &[0, 2, 1]);
                            handled_twiddle = true;
                        }
                        "••🔀" | "swap-deep" => {
                            vs.apply_shuffle(4, &[0, 1, 3, 2]);
                            handled_twiddle = true;
                        }
                        "⚓" | "sink" => {
                            vs.apply_shuffle(3, &[1, 2, 0]);
                            handled_twiddle = true;
                        }
                        "•⚓" | "sink-down" => {
                            vs.apply_shuffle(4, &[0, 2, 3, 1]);
                            handled_twiddle = true;
                        }
                        "••⚓" | "sink-deep" => {
                            vs.apply_shuffle(5, &[0, 1, 3, 4, 2]);
                            handled_twiddle = true;
                        }
                        "🛟" | "float" => {
                            vs.apply_shuffle(3, &[2, 0, 1]);
                            handled_twiddle = true;
                        }
                        "•🛟" | "float-down" => {
                            vs.apply_shuffle(4, &[0, 3, 1, 2]);
                            handled_twiddle = true;
                        }
                        "••🛟" | "float-deep" => {
                            vs.apply_shuffle(5, &[0, 1, 4, 2, 3]);
                            handled_twiddle = true;
                        }
                        _ => {}
                    }
                }

                if handled_twiddle {
                    continue;
                }

                // Check for pure shuffle inlining if dict is available

                if let Some(entry) = dict.get_entry(w) {
                    if let Executable::Derived(ref chunk) = entry.definition {
                        if let Some((pops, pushes)) = chunk_to_shuffle(chunk) {
                            vs.apply_shuffle(pops as usize, &pushes);
                            continue;
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
                // `🪄` (dip) combinator: Pops a block, pops the TOS, runs the block, restores the TOS.
                // If the inner block is purely stack shuffles, we can map it to a single inline shuffle
                // by shifting all internal indices up by 1 and maintaining TOS (index 0) identically.
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
                // `•🪄` (dipdown) combinator: Pops a block, hides TOS & NOS, runs block, restores them.
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
                        let dip_chunk = Chunk {
                            ops: vec![Op::Dip(std::sync::Arc::new(inner_chunk)), Op::Return],
                            source: None,
                        };
                        ops.push(Op::Dip(std::sync::Arc::new(dip_chunk)));
                        continue;
                    }
                // `••🪄` (dipdeep) combinator: Hides the top 3 elements before running the block.
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
                        let dip_chunk1 = Chunk {
                            ops: vec![Op::Dip(std::sync::Arc::new(inner_chunk)), Op::Return],
                            source: None,
                        };
                        let dip_chunk2 = Chunk {
                            ops: vec![Op::Dip(std::sync::Arc::new(dip_chunk1)), Op::Return],
                            source: None,
                        };
                        ops.push(Op::Dip(std::sync::Arc::new(dip_chunk2)));
                        continue;
                    }
                } else if w_str == "▶️" || w_str == "evaluate" {
                    let len = ops.len();
                    if len >= 2 {
                        if let (Op::Call(w_prev), Op::Push(crate::types::Item::List(l))) =
                            (&ops[len - 1], &ops[len - 2])
                        {
                            if w_prev.data.as_str() == "🛡️" || w_prev.data.as_str() == "shield"
                            {
                                let l_clone = l.clone();
                                ops.pop();
                                ops.pop();
                                let mut inner_chunk = compile_with_dict(&l_clone, dict);
                                inner_chunk.ops.pop(); // remove implicit return
                                ops.push(Op::Shield(std::sync::Arc::new(inner_chunk)));
                                continue;
                            }
                        }
                    }
                    ops.push(Op::Call(w.clone()));
                    continue;
                // `↔️` (branch) combinator: Pops a boolean condition and two blocks. Executes one based on the condition.
                // We compile this to `JumpIfFalseKeepIfTrue` and `Jump` instructions for native performance.
                } else if w_str == "↔️" {
                    let len = ops.len();
                    if len >= 2 {
                        if let (Op::Push(Item::List(f)), Op::Push(Item::List(t))) =
                            (&ops[len - 1], &ops[len - 2])
                        {
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
                            ops[jump_base_idx] = Op::JumpIfFalseKeepIfTrue(
                                (base_case_idx as isize) - (jump_base_idx as isize) - 1,
                            );

                            let mut false_chunk = compile_with_dict(&f_clone, dict);
                            false_chunk.ops.pop();
                            ops.extend(false_chunk.ops);

                            let end_idx = ops.len();
                            ops[jump_start_idx] =
                                Op::Jump((end_idx as isize) - (jump_start_idx as isize) - 1);

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
    Chunk {
        ops,
        source: Some(list.clone()),
    }
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

pub fn compile_recur_with_dict(
    pred: &cont::List,
    true_branch: &cont::List,
    false_branch: &cont::List,
    combinator: &cont::List,
    dict: &Dictionary,
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
        ops[jump_base_idx] =
            Op::JumpIfFalseKeepIfTrue((base_case_idx as isize) - (jump_base_idx as isize) - 1);

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
    ops[jump_base_idx] =
        Op::JumpIfFalseKeepIfTrue((base_case_idx as isize) - (jump_base_idx as isize) - 1);

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
        ops[jump_unwind_end_idx] = Op::JumpIfLoopCounterZero(
            0,
            (unwind_end_idx as isize) - (jump_unwind_end_idx as isize) - 1,
        );

        ops.push(Op::PopLoopCounter);
    }
    ops.push(Op::Return);
    Chunk { ops, source: None }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::container::program::Op;
    use crate::types::container::List;
    use crate::types::Item;
    use crate::types::WordData;
    use internment::Intern;

    fn check_compile(words: &str, expected_pops: u8, expected_pushes: &[u8]) {
        let mut list = List::new();
        for word in words.split_whitespace() {
            list.push_back(Item::Word(Intern::new(WordData {
                data: Intern::new(word.to_string()),
                quoted: false,
                namespace: None,
            })));
        }
        let chunk = compile_with_dict(&list, &crate::types::container::dictionary::Dictionary::default());
        if expected_pops == 0 && expected_pushes.is_empty() {
            assert_eq!(chunk.ops.len(), 1, "Expected no shuffle for {}", words);
            assert!(matches!(chunk.ops[0], Op::Return));
            return;
        }
        assert_eq!(chunk.ops.len(), 2, "Expected 2 ops for {}", words);
        if let Op::Shuffle { pops, pushes } = &chunk.ops[0] {
            assert_eq!(*pops, expected_pops, "pops mismatch for {}", words);
            assert_eq!(
                pushes.as_slice(),
                expected_pushes,
                "pushes mismatch for {}",
                words
            );
        } else {
            panic!("Expected Shuffle for {}, got {:?}", words, chunk.ops[0]);
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

        check_compile("float", 3, &[1, 0, 2]); // Original DEEPEST-to-TOS for float: [1, 0, 2]
        check_compile("float-down", 4, &[2, 1, 3, 0]);

        // Composites
        check_compile("swap drop", 2, &[0]);
        check_compile("dup drop", 0, &[]);
        check_compile("dup swap-down drop", 2, &[0, 1]); // Equivalent to swap
        check_compile("dup float drop", 2, &[0, 0]); // Mathematically verified to be dup
    }
}
