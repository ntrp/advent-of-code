use crate::problem::{self, Solution};

pub struct Day<<DAY>>;

impl Solution for Day<<DAY>> {
    fn name(&self) -> &'static str {
        "n/a"
    }

    fn part_a(&self) -> String {
        let data = load();
        todo!()
    }

    fn part_b(&self) -> String {
        let data = load();
        todo!()
    }
}

fn load() -> () {
    let data = problem::load(<<DAYSHORT>>);
    todo!()
}

#[cfg(test)]
mod tests {

    #[test]
    fn part_a_test() {
        let input = String::from("input");

        //assert_eq!(val, fn);
    }

    #[test]
    fn part_b_test() {
        let input = String::from("input");

        //assert_eq!(val, fn);
    }
}
