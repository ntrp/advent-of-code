use std::cmp::Reverse;

use nom::{
    bytes::complete::tag, character::complete::line_ending, combinator::map, multi::separated_list1,
};

use crate::{
    parsers::decimal,
    problem::{self, Solution},
};

pub struct Day01;

impl Solution for Day01 {
    fn name(&self) -> &'static str {
        "Calorie Counting"
    }

    fn part_a(&self, test: bool) -> String {
        let elfs = load(test);
        match elfs.iter().max() {
            Some(val) => val.to_string(),
            None => todo!(),
        }
    }

    fn part_b(&self, test: bool) -> String {
        let mut elfs = load(test);
        elfs.sort_by_key(|w| Reverse(*w));
        elfs.iter().take(3).sum::<u32>().to_string()
    }
}

fn load(test: bool) -> Vec<u32> {
    let data = problem::load(1, test);
    let numbers = map(
        separated_list1(line_ending, decimal),
        |calories: Vec<u32>| calories.iter().sum::<u32>(),
    );
    let elfs = separated_list1(tag("\n\n"), numbers)(data.as_str());
    match elfs {
        Ok((_, nums)) => nums.clone(),
        Err(_) => todo!(),
    }
}
