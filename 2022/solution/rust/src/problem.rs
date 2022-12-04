use std::fs;

pub trait Solution {
    fn name(&self) -> &'static str;
    fn part_a(&self, test: bool) -> String;
    fn part_b(&self, test: bool) -> String;
}

pub fn load(day: u8, test: bool) -> String {
    let file_name = if test {"test"} else {"input"};
    let file = format!("../../../2022/days/{:02}/{file_name}.txt", day);
    fs::read_to_string(&file).unwrap_or_else(|_| panic!("Error reading file {}", file))
}
