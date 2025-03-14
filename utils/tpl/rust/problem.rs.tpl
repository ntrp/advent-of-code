use std::fs;

pub trait Solution {
    fn name(&self) -> &'static str;
    fn part_a(&self) -> String;
    fn part_b(&self) -> String;
}

pub fn load(day: u8) -> String {
    let file = format!("../../../<<YEAR>>/days/{:02}/input.txt", day);
    fs::read_to_string(&file).unwrap_or_else(|_| panic!("Error reading file {}", file))
}
