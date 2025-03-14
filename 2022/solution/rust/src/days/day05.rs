use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, digit1, newline},
    combinator::map,
    error::ErrorKind,
    multi::{many1, separated_list1},
    sequence::{delimited, tuple},
};

use crate::{
    parsers::{decimal, ws},
    problem::{self, Solution},
};

pub struct Day05;

impl Solution for Day05 {
    fn name(&self) -> &'static str {
        "Supply Stacks"
    }

    fn part_a(&self) -> String {
        let LoadResult(table, instructions) = load();
        let mut state = transpose_to_stack(table);
        for (amount, src, dst) in instructions {
            for _ in 0..amount {
                let src_val = state[(src - 1) as usize].pop().unwrap();
                state[(dst - 1) as usize].push(src_val);
            }
        }
        state
            .iter()
            .map(|curr| curr.last().unwrap().clone())
            .collect::<Vec<String>>()
            .join("")
    }

    fn part_b(&self) -> String {
        let LoadResult(table, instructions) = load();
        let mut state = transpose_to_stack(table);
        for (amount, src, dst) in instructions {
            let mut buff = vec![];
            for _ in 0..amount {
                let src_val = state[(src - 1) as usize].pop().unwrap();
                buff.push(src_val);
            }
            for elem in buff.iter().rev() {
                state[(dst - 1) as usize].push(elem.clone());
            }
        }
        state
            .iter()
            .map(|curr| curr.last().unwrap().clone())
            .collect::<Vec<String>>()
            .join("")
    }
}

struct LoadResult(Vec<Vec<String>>, Vec<(u32, u32, u32)>);

fn load() -> LoadResult {
    let data = problem::load(5);
    let crate_symbol = map(
        delimited(char::<_, (&str, ErrorKind)>('['), alpha1, char(']')),
        |v| v.to_owned(),
    );
    let empty_symbol = map(tag::<_, _, (&str, ErrorKind)>("   "), |_| "".to_owned());
    let table_line = separated_list1(char(' '), alt((empty_symbol, crate_symbol)));
    let table = separated_list1(newline, table_line);

    let table_idx = many1(ws(digit1));

    let instruction_line = map(
        tuple((
            ws(tag("move")),
            decimal,
            ws(tag("from")),
            decimal,
            ws(tag("to")),
            decimal,
        )),
        |(_, amount, _, src, _, dst)| (amount, src, dst),
    );
    let mut instructions = separated_list1(newline, instruction_line);

    let mut res = tuple((table, newline, table_idx));

    match res(data.as_str()) {
        Ok((rest, (tbl, _, _))) => {
            let (_, instr) = instructions(rest).unwrap().clone();
            LoadResult(tbl, instr)
        }
        Err(_) => todo!(),
    }
}

fn transpose_to_stack(table: Vec<Vec<String>>) -> Vec<Vec<String>> {
    let mut res: Vec<Vec<String>> = vec![];
    for _ in 0..table[0].len() {
        res.push(vec![]);
    }
    for row in table.iter().rev() {
        for (i, elem) in row.iter().enumerate() {
            if !elem.is_empty() {
                res[i].push(elem.clone())
            }
        }
    }
    res
}

#[cfg(test)]
mod tests {
    use crate::days::day05::transpose_to_stack;

    #[test]
    fn transpose_test() {
        let input = [
            ["".to_owned(), "a".to_owned(), "".to_owned()].to_vec(),
            ["b".to_owned(), "a".to_owned(), "".to_owned()].to_vec(),
            ["b".to_owned(), "a".to_owned(), "c".to_owned()].to_vec(),
        ]
        .to_vec();

        assert_eq!(
            [
                ["b", "b"].to_vec(),
                ["a", "a", "a"].to_vec(),
                ["c"].to_vec()
            ]
            .to_vec(),
            transpose_to_stack(input)
        );
    }
}
