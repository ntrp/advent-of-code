use std::collections::HashSet;

use nom::{
    character::complete::{digit1, newline, one_of},
    combinator::{map, map_res},
    multi::separated_list1,
    sequence::tuple,
    IResult,
};

use crate::{
    parsers::ws,
    problem::{self, Solution},
};

pub struct Day09;

impl Solution for Day09 {
    fn name(&self) -> &'static str {
        "Rope Bridge"
    }

    fn part_a(&self) -> String {
        let commands = load();
        let mut rope = std::iter::repeat_n((0, 0), 2).collect::<Vec<(i32, i32)>>();
        let mut t_positions: HashSet<(i32, i32)> = HashSet::new();
        for command in commands {
            for _ in 0..command.value {
                t_positions.insert(Clone::clone(rope.get(1).unwrap()));
                move_head(rope.get_mut(0).unwrap(), &command.direction);
                move_knot(&mut rope, 0);
            }
        }
        t_positions.len().to_string()
    }

    fn part_b(&self) -> String {
        let commands = load();
        let mut rope = std::iter::repeat_n((0, 0), 10).collect::<Vec<(i32, i32)>>();
        let mut t_positions: HashSet<(i32, i32)> = HashSet::new();
        for command in commands {
            for _ in 0..command.value {
                move_head(rope.get_mut(0).unwrap(), &command.direction);
                for i in 0..(rope.len() - 1) {
                    move_knot(&mut rope, i);
                }
                t_positions.insert(*rope.last().unwrap());
            }
        }
        t_positions.len().to_string()
    }
}

#[allow(dead_code)]
fn debug_rope(rope: Vec<(i32, i32)>, size: i32) {
    for y in (-size..size).rev() {
        for x in -size..size {
            if x == 0 && y == 0 {
                print!("s ");
            } else {
                print!(
                    "{} ",
                    rope.iter()
                        .find(|(i, j)| *i == x && *j == y)
                        .map(|_| "#")
                        .unwrap_or_else(|| ".")
                );
            }
        }
        println!();
    }
}

fn move_head(h: &mut (i32, i32), dir: &Direction) {
    match dir {
        Direction::U => h.1 += 1,
        Direction::R => h.0 += 1,
        Direction::D => h.1 -= 1,
        Direction::L => h.0 -= 1,
    };
}

fn move_knot(rope: &mut [(i32, i32)], i: usize) {
    let h = *rope.get(i).unwrap();
    let t = rope.get_mut(i + 1).unwrap();
    if h.1.abs_diff(t.1) > 1 {
        t.1 = t.1 + if h.1 > t.1 { 1 } else { -1 };
        if h.0.abs_diff(t.0) >= 1 {
            t.0 = t.0 + if h.0 > t.0 { 1 } else { -1 };
        }
    } else if h.0.abs_diff(t.0) > 1 {
        t.0 = t.0 + if h.0 > t.0 { 1 } else { -1 };
        if h.1.abs_diff(t.1) >= 1 {
            t.1 = t.1 + if h.1 > t.1 { 1 } else { -1 };
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Direction {
    U,
    L,
    D,
    R,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DirectionCommand {
    pub direction: Direction,
    pub value: usize,
}

fn parse_line(data: &str) -> IResult<&str, DirectionCommand> {
    map(
        tuple((
            map(ws(one_of("ULDR")), |text| match text {
                'U' => Direction::U,
                'L' => Direction::L,
                'D' => Direction::D,
                'R' => Direction::R,
                err => panic!("Direction {} does not exist", err),
            }),
            map_res(digit1, str::parse::<usize>),
        )),
        |(dir, val)| DirectionCommand {
            direction: dir,
            value: val,
        },
    )(data)
}

fn load() -> Vec<DirectionCommand> {
    let data = problem::load(9);
    let (_, commands) = separated_list1(newline, parse_line)(data.as_str()).unwrap();
    commands
}

#[cfg(test)]
mod tests {
    use crate::days::day09::{parse_line, Direction, DirectionCommand};

    #[test]
    fn parse_line_test() {
        let input = "L 4";

        assert_eq!(
            parse_line(input).unwrap().1,
            DirectionCommand {
                direction: Direction::L,
                value: 4
            }
        );
    }

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
