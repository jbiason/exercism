// This stub file contains items which aren't used yet; feel free to remove this module attribute
// to enable stricter warnings.
#![allow(unused)]

use std::collections::HashMap;

pub fn can_construct_note(magazine: &[&str], note: &[&str]) -> bool {
    let mut words_on_magazine: HashMap<&str, u16> = HashMap::new();
    magazine.iter().for_each(|word| {
        let count = if words_on_magazine.contains_key(word) {
            *words_on_magazine.get(word).unwrap()
        } else {
            0
        };
        words_on_magazine.insert(word, count + 1);
    });

    for note_word in note {
        if !words_on_magazine.contains_key(note_word) {
            return false;
        } else {
            let count = words_on_magazine.get(note_word).unwrap() - 1;
            if count > 0 {
                words_on_magazine.insert(note_word, count);
            } else {
                words_on_magazine.remove(note_word);
            }
        }
    }
    true
}
