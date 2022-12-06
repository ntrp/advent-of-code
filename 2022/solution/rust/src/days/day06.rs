use std::{collections::HashSet, usize};

use crate::problem::{self, Solution};

pub struct Day06;

impl Solution for Day06 {
    fn name(&self) -> &'static str {
        "Tuning Trouble"
    }

    fn part_a(&self) -> String {
        let stream = load();
        find_marker(stream, 4).to_string()
    }

    fn part_b(&self) -> String {
        let stream = load();
        find_marker(stream, 14).to_string()
    }
}

fn load() -> String {
    problem::load(6)
}

fn find_marker(stream: String, size: usize) -> usize {
    let mut i: usize = 0;
    while i < (stream.len() - size) {
        let slice = &stream[i..i + size];
        let set: HashSet<char> = HashSet::from_iter(slice.chars());
        if set.len() == size {
            break;
        }
        i += 1;
    }
    i + size
}

#[cfg(test)]
mod tests {
    use crate::days::day06::find_marker;


    #[test]
    fn part_a_test() {
        let input = String::from("mjqjpqmgbljsphdztnvjfqwrcgsmlb");

        assert_eq!(7, find_marker(input, 4));
    }

    #[test]
    fn part_b_test() {
        let input = String::from("mjqjpqmgbljsphdztnvjfqwrcgsmlb");

        assert_eq!(19, find_marker(input, 14));
    }
}
