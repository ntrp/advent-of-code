use std::collections::{HashMap, VecDeque};

use crate::{
    parsers::{big_decimal, decimal, ws},
    problem::{self, Solution},
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, one_of},
    combinator::map,
    multi::separated_list1,
    sequence::{delimited, preceded, tuple},
    IResult,
};

pub struct Day11;

impl Solution for Day11 {
    fn name(&self) -> &'static str {
        "n/a"
    }

    fn part_a(&self) -> String {
        let mut monkeys = load();
        solve(&mut monkeys, 20, 0)
    }

    fn part_b(&self) -> String {
        let mut monkeys = load();
        let modulo = monkeys.iter().map(|monkey| monkey.cond_div_by).fold(1, |prev, val| prev * val);
        solve(&mut monkeys, 10000, modulo)
    }
}

fn solve(monkeys: &mut Vec<Monkey>, rounds: usize, modulo: u128) -> String {
    let mut inspect_map: HashMap<u128, u128> = Default::default();
    for _round in 0..rounds {
        for i in 0..monkeys.len() {
            let monkey = monkeys.get_mut(i).unwrap();
            let len = monkey.items.len() as u128;
            inspect_map
                .entry(monkey.id)
                .and_modify(|val| *val += len)
                .or_insert(len);
            let mut change_map: HashMap<u128, Vec<u128>> = Default::default();
            while let Some(item) = monkey.items.pop_front() {
                let mut worry_lvl: u128 = match monkey.operation.op {
                    Op::PLUS => item + monkey.operation.val.clone(),
                    Op::MULTIPLY => item * monkey.operation.val.clone(),
                    Op::SQUARE => item.clone() * item.clone(),
                };
                if modulo > 0 {
                    worry_lvl %= modulo;
                } else {
                    worry_lvl /= 3;
                }
                let dest = if worry_lvl % monkey.cond_div_by == 0 {
                    monkey.cond_if_true
                } else {
                    monkey.cond_if_false
                };
                change_map
                    .entry(dest)
                    .or_insert_with(Vec::new)
                    .push(worry_lvl);
            }
            drop(monkey);
            for (id, items) in change_map {
                let monkey = monkeys.get_mut(id as usize).unwrap();
                for item in items {
                    monkey.items.push_back(item);
                }
            }
        }
    }

    let mut vals = inspect_map
        .values()
        .map(|v| v.clone())
        .collect::<Vec<u128>>();
    vals.sort();
    let a = vals.pop().unwrap();
    let b = vals.pop().unwrap();
    println!("{:?}", inspect_map);
    (a * b).to_string()
}

#[derive(Debug, PartialEq, Eq)]
pub struct FactorizedSum {}

#[derive(Debug, PartialEq, Eq)]
enum Op {
    PLUS,
    MULTIPLY,
    SQUARE,
}

#[derive(Debug, PartialEq, Eq)]
struct Operation {
    op: Op,
    val: u128,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Monkey {
    id: u128,
    items: VecDeque<u128>,
    operation: Operation,
    cond_div_by: u128,
    cond_if_true: u128,
    cond_if_false: u128,
}

fn parse_operation(data: &str) -> IResult<&str, Operation> {
    map(
        tuple((ws(one_of("+-*")), alt((tag("old"), digit1)))),
        |(sign, val)| Operation {
            op: match sign {
                '+' => Op::PLUS,
                '*' => {
                    if val == "old" {
                        Op::SQUARE
                    } else {
                        Op::MULTIPLY
                    }
                }
                _ => panic!(),
            },
            val: if val == "old" {
                0
            } else {
                str::parse(val).unwrap()
            },
        },
    )(data)
}

fn parse_data(data: &str) -> IResult<&str, Vec<Monkey>> {
    separated_list1(
        tag("\n\n"),
        map(
            tuple((
                delimited(tag("Monkey "), big_decimal, tag(":\n")),
                preceded(
                    ws(tag("Starting items: ")),
                    separated_list1(tag(", "), big_decimal),
                ),
                preceded(ws(tag("Operation: new = old ")), parse_operation),
                preceded(ws(tag("Test: divisible by ")), big_decimal),
                preceded(ws(tag("If true: throw to monkey ")), big_decimal),
                preceded(ws(tag("If false: throw to monkey ")), big_decimal),
            )),
            |(id, items, operation, cond_div_by, cond_if_true, cond_if_false)| Monkey {
                id,
                items: VecDeque::from(items),
                operation,
                cond_div_by,
                cond_if_true,
                cond_if_false,
            },
        ),
    )(data)
}

fn load() -> Vec<Monkey> {
    let data = problem::load(11);
    let (_, res) = parse_data(data.as_str()).unwrap();
    res
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
