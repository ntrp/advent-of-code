use std::collections::HashSet;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{i32, newline},
    combinator::map,
    multi::separated_list1,
    sequence::preceded,
    IResult,
};

use crate::{
    parsers::ws,
    problem::{self, Solution},
};

pub struct Day10;

impl Solution for Day10 {
    fn name(&self) -> &'static str {
        "n/a"
    }

    fn part_a(&self) -> String {
        let commands = load();
        let relevant_cycles: HashSet<i32> = (0..10).map(|val| (val * 40) + 20).collect();
        let mut total_power = 0i32;
        let mut register = 1i32;
        let mut cycle = 1i32;
        for command in commands {
            if relevant_cycles.contains(&cycle) {
                total_power += register * cycle;
            }
            match command {
                Command::Noop {} => cycle += 1,
                Command::Addx { val } => {
                    cycle += 1;
                    if relevant_cycles.contains(&cycle) {
                        total_power += register * cycle;
                    }
                    cycle += 1;
                    register += val;
                }
            }
        }
        total_power.to_string()
    }

    fn part_b(&self) -> String {
        let commands = load();
        let mut register = 1i32;
        let mut cycle = 0i32;
        let mut pixels: Vec<&str> = vec![];
        for command in commands {
            pixels.push(if (cycle % 40).abs_diff(register) <= 1 {
                "#"
            } else {
                "."
            });
            match command {
                Command::Noop {} => cycle += 1,
                Command::Addx { val } => {
                    cycle += 1;
                    pixels.push(if (cycle % 40).abs_diff(register) <= 1 {
                        "#"
                    } else {
                        "."
                    });
                    cycle += 1;
                    register += val;
                }
            }
        }
        for i in 0..6 {
            for j in 0..40 {
                print!("{}", pixels.get(i * 40 + j).unwrap_or(&"."));
            }
            println!()
        }
        "".to_string()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    Noop {},
    Addx { val: i32 },
}

fn parse_command(data: &str) -> IResult<&str, Command> {
    alt((
        map(tag("noop"), |_| Command::Noop {}),
        map(preceded(ws(tag("addx")), i32), |n| Command::Addx { val: n }),
    ))(data)
}

fn load() -> Vec<Command> {
    let data = problem::load(10);
    let (_, commands) = separated_list1(newline, parse_command)(data.as_str()).unwrap();
    commands
}

#[cfg(test)]
mod tests {

    #[test]
    fn part_a_test() {
        //let input = String::from("input");

        //assert_eq!(val, fn);
    }

    #[test]
    fn part_b_test() {
        //let input = String::from("input");

        //assert_eq!(val, fn);
    }
}
