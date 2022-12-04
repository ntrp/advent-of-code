use crate::problem::Solution;

mod day01;
mod day02;
mod day03;

pub const ALL: [&dyn Solution; 3] = [&day01::Day01, &day02::Day02, &day03::Day03];
