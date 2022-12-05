use nom::{
    character::complete::{char, newline},
    combinator::map,
    multi::separated_list1,
    sequence::tuple,
};

use crate::{
    parsers::decimal,
    problem::{self, Solution},
};

pub struct Day04;

impl Solution for Day04 {
    fn name(&self) -> &'static str {
        "Camp Cleanup"
    }

    fn part_a(&self, test: bool) -> String {
        let assignments = load(test);
        assignments
            .iter()
            .filter(|((a, b), (c, d))| (a >= c && b <= d) || (c >= a && d <= b))
            .count()
            .to_string()
    }

    fn part_b(&self, test: bool) -> String {
        let assignments = load(test);
        assignments
            .iter()
            .filter(|((a, b), (c, d))| b >= c && a <= d)
            .count()
            .to_string()
    }
}

fn load(test: bool) -> Vec<((u32, u32), (u32, u32))> {
    let data = problem::load(4, test);
    let assignment = map(
        tuple((
            decimal,
            char('-'),
            decimal,
            char(','),
            decimal,
            char('-'),
            decimal,
        )),
        |(a, _, b, _, c, _, d)| ((a, b), (c, d)),
    );
    let mut pairs = separated_list1(newline, assignment);
    match pairs(data.as_str()) {
        Ok((_, res)) => res,
        Err(err) => panic!("{}", err),
    }
}
