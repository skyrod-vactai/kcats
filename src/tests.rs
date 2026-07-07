#[cfg(test)]
mod tests {
    use crate::compile::{compile, VirtualStack};
    use crate::types::container::program::{Chunk, Op};
    use crate::types::Item;
    use crate::types::container::List;

    fn check_compile(word: &str, expected_pops: u8, expected_pushes: &[u8]) {
        let mut list = List::new();
        list.push_back(Item::Word(crate::types::Intern::new(word.to_string())));
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
        check_compile("swap", 2, &[1, 0]); // Wait! I want it to be [1, 0]? Let's see what it is right now.
        check_compile("dup", 1, &[0, 0]);
        check_compile("sink", 2, &[1, 0]); 
    }
}
