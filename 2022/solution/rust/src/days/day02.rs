use nom::{
    character::complete::{newline, one_of, space1},
    combinator::map,
    error::ErrorKind,
    multi::separated_list1,
    sequence::tuple,
};

use crate::problem::{self, Solution};

pub struct Day02;

impl Solution for Day02 {
    fn name(&self) -> &'static str {
        "Rock Paper Scissors"
    }

    fn part_a(&self) -> String {
        let rounds = load();
        rounds.iter().map(|(him, me)| compute_score(*him, *me)).sum::<usize>().to_string()
    }

    fn part_b(&self) -> String {
        let rounds = load();
        rounds.iter().map(|(him, me)| compute_score(*him, prepare(*him, *me))).sum::<usize>().to_string()
    }
}

const HIM: &str = "ABC";
const ME: &str = "XYZ";

fn load() -> Vec<(usize, usize)> {
    let data = problem::load(2);
    let round = map(
        tuple((
            one_of::<_, _, (&str, ErrorKind)>(HIM),
            space1,
            one_of::<_, _, (&str, ErrorKind)>(ME),
        )),
        |(him, _, me)| (HIM.find(him).unwrap(), ME.find(me).unwrap()),
    );
    let mut rounds = separated_list1(newline, round);
    match rounds(data.as_str()) {
        Ok((_, res)) => res,
        Err(_) => todo!(),
    }
}

fn prepare(him: usize, me: usize) -> usize {
    if me == 0 {
        (him as i32 - 1).rem_euclid(3) as usize
    } else if me == 1 {
        him
    } else {
        (him + 1).rem_euclid(3)
    }
}

fn compute_score(him: usize, me: usize) -> usize {
    let mut score = me + 1;
    if him == me {
        score += 3
    } else if me == ((him + 1) % 3) {
        score += 6
    }
    score
}
