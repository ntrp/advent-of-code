use std::collections::HashSet;

use nom::{
    character::complete::{alpha1, newline},
    combinator::map,
    error::ErrorKind,
    multi::separated_list1,
};

use crate::problem::{self, Solution};

pub struct Day03;

impl Solution for Day03 {
    fn name(&self) -> &'static str {
        "Rucksack Reorganization"
    }

    fn part_a(&self) -> String {
        let sacks = load();
        sacks
            .iter()
            .map(|prios| {
                let mut first = prios.clone();
                let last = first.split_off(first.len() / 2);
                let res = first
                    .iter()
                    .filter(|elem| last.contains(elem))
                    .collect::<HashSet<_>>();
                if res.len() == 1 {
                    **res.iter().next().unwrap() as u32
                } else {
                    panic!("Found more than one duplicated elements")
                }
            })
            .sum::<u32>()
            .to_string()
    }

    fn part_b(&self) -> String {
        let sacks = load();
        sacks
            .iter()
            .fold((vec![], 0u32), |(mut buff, sum), curr| {
                if buff.len() < 3 {
                    buff.push(curr)
                };
                if buff.len() == 3 {
                    let res = buff[0].clone();
                    let res = res
                        .iter()
                        .filter(|elem| buff[1].contains(elem))
                        .filter(|elem| buff[2].contains(elem))
                        .collect::<HashSet<_>>();
                    println!("{:?}", res);
                    (vec![], sum + (**res.iter().next().unwrap() as u32))
                } else {
                    (buff, sum)
                }
            })
            .1
            .to_string()
    }
}

fn load() -> Vec<Vec<u8>> {
    let data = problem::load(3);
    let mut sacks = separated_list1(
        newline::<_, (&str, ErrorKind)>,
        map(alpha1, |sack: &str| {
            sack.chars().map(map_priority).collect::<Vec<_>>()
        }),
    );
    match sacks(data.as_str()) {
        Ok((_, res)) => res,
        Err(err) => panic!("{}", err),
    }
}

fn map_priority(item_type: char) -> u8 {
    let code = item_type as u8;
    if code < 91 {
        code - 38
    } else {
        code - 96
    }
}

#[cfg(test)]
mod tests {
    use crate::days::day03::map_priority;

    #[test]
    fn map_priority_test() {
        assert_eq!(1u8, map_priority('a'));
        assert_eq!(26u8, map_priority('z'));
        assert_eq!(27u8, map_priority('A'));
        assert_eq!(52u8, map_priority('Z'));
    }
}
