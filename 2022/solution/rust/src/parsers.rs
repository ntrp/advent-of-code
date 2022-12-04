use nom::{IResult, character::complete::digit1, combinator::map_res};

pub fn decimal(input: &str) -> IResult<&str, u32> {
  map_res(digit1, str::parse)(input)
}
