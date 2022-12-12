use nom::{
    character::complete::{digit1, multispace0},
    combinator::map_res,
    error::ParseError,
    sequence::delimited,
    IResult,
};

pub fn decimal(input: &str) -> IResult<&str, u32> {
    map_res(digit1, str::parse)(input)
}

pub fn big_decimal(input: &str) -> IResult<&str, u128> {
    map_res(digit1, str::parse)(input)
}

pub fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}
