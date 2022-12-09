use nom::character::complete::{newline, satisfy};
use nom::{
    combinator::map,
    multi::{many1, separated_list1},
    AsChar, IResult,
};

use crate::problem::{self, Solution};

pub struct Day08;

impl Solution for Day08 {
    fn name(&self) -> &'static str {
        "Treetop Tree House"
    }

    fn part_a(&self) -> String {
        let arr = load();
        let mut count = arr[0].len() * 2 + arr.len() * 2 - 4;
        for i in 1..(arr[0].len() - 1) {
            for j in 1..(arr.len() - 1) {
                if !tree_hidden(i, j, &arr) {
                    count += 1
                }
            }
        }
        count.to_string()
    }

    fn part_b(&self) -> String {
        let arr = load();
        let mut score = 0;
        for i in 1..(arr[0].len() - 1) {
            for j in 1..(arr.len() - 1) {
                let res = tree_scenic_score(i, j, &arr);
                if res > score {
                    score = res;
                }
            }
        }
        score.to_string()
    }
}

fn parse_line(data: &str) -> IResult<&str, Vec<u32>> {
    many1(map(satisfy(|c: char| c.is_dec_digit()), |c| {
        c.to_digit(10).unwrap()
    }))(data)
}

fn tree_hidden(i: usize, j: usize, matrix: &Vec<Vec<u32>>) -> bool {
    let current = matrix[j][i];
    let east = ((i + 1)..matrix[0].len()).find(|new_i| matrix[j][*new_i] >= current);
    if east.is_some() {
        let south = ((j + 1)..matrix.len()).find(|new_j| matrix[*new_j][i] >= current);
        if south.is_some() {
            let west = (0..i).rev().find(|new_i| matrix[j][*new_i] >= current);
            if west.is_some() {
                let north = (0..j).rev().find(|new_j| matrix[*new_j][i] >= current);
                return north.is_some();
            }
        }
    }
    false
}

fn direction_scenic_score(
    indices: Vec<(usize, usize)>,
    current: u32,
    matrix: &Vec<Vec<u32>>,
) -> usize {
    let lower_trees = indices
        .iter()
        .take_while(|(j, i)| matrix[*j][*i] < current)
        .count();
    if lower_trees < indices.len() {
        lower_trees + 1
    } else {
        lower_trees
    }
}

fn tree_scenic_score(i: usize, j: usize, matrix: &Vec<Vec<u32>>) -> usize {
    let current = matrix[j][i];
    let width = matrix[0].len();
    let height = matrix.len();
    let east = direction_scenic_score(
        ((i + 1)..width).map(|new_i| (j, new_i)).collect(),
        current,
        matrix,
    );
    let south = direction_scenic_score(
        ((j + 1)..height).map(|new_j| (new_j, i)).collect(),
        current,
        matrix,
    );
    let west = direction_scenic_score(
        (0..i).rev().map(|new_i| (j, new_i)).collect(),
        current,
        matrix,
    );
    let north = direction_scenic_score(
        (0..j).rev().map(|new_j| (new_j, i)).collect(),
        current,
        matrix,
    );
    east * south * west * north
}

fn load() -> Vec<Vec<u32>> {
    let data = problem::load(8);
    let (_, arr) = separated_list1(newline, parse_line)(data.as_str()).unwrap();
    arr
}

#[cfg(test)]
mod tests {
    use nom::{character::complete::newline, multi::separated_list1};

    use crate::days::day08::{parse_line, tree_hidden, tree_scenic_score};

    #[test]
    fn parse_line_test() {
        let input = "123456";

        assert_eq!(vec![1, 2, 3, 4, 5, 6], parse_line(input).unwrap().1);
    }

    #[test]
    fn check_tree_test() {
        let input = "30373
25512
65332
33549
35390";

        let (_, arr) = separated_list1(newline, parse_line)(input).unwrap();

        assert_eq!(true, tree_hidden(3, 1, &arr));
        assert_eq!(false, tree_hidden(1, 1, &arr));
    }

    #[test]
    fn tree_scenic_score_test() {
        let input = "30373
25512
65332
33549
35390";

        let (_, arr) = separated_list1(newline, parse_line)(input).unwrap();

        assert_eq!(4, tree_scenic_score(2, 1, &arr));
        assert_eq!(8, tree_scenic_score(2, 3, &arr));
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
