use crate::problem::Solution;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
mod day25;

pub const ALL: [&dyn Solution; 25] = [
    &day01::Day01,
    &day02::Day02,
    &day03::Day03,
    &day04::Day04,
    &day05::Day05,
    &day06::Day06,
    &day07::Day07,
    &day08::Day08,
    &day09::Day09,
    &day10::Day10,
    &day11::Day11,
    &day12::Day12,
    &day13::Day13,
    &day14::Day14,
    &day15::Day15,
    &day16::Day16,
    &day17::Day17,
    &day18::Day18,
    &day19::Day19,
    &day20::Day20,
    &day21::Day21,
    &day22::Day22,
    &day23::Day23,
    &day24::Day24,
    &day25::Day25,
];
