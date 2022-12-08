use std::collections::HashMap;

use crate::{
    parsers::{decimal, ws},
    problem::{self, Solution},
};
use indextree::{Arena, NodeId};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{
        complete::alpha1,
        complete::{char, newline, not_line_ending},
    },
    combinator::map,
    multi::separated_list1,
    sequence::{preceded, tuple},
    IResult,
};

pub struct Day07;

impl Solution for Day07 {
    fn name(&self) -> &'static str {
        "No Space Left On Device"
    }

    fn part_a(&self) -> String {
        let (root, fs) = load();
        let mut memo = HashMap::new();
        calc_dir_sizes(&mut memo, root, &fs);
        memo.iter()
            .filter(|(_, size)| **size <= 100000)
            .map(|(_, size)| size)
            .sum::<u32>()
            .to_string()
    }

    fn part_b(&self) -> String {
        let (root, fs) = load();
        let mut memo = HashMap::new();
        let total_usage = calc_dir_sizes(&mut memo, root, &fs);
        let free_space = 70000000 - total_usage;
        let need_to_free = 30000000 - free_space;
        let mut sizes = memo.iter().map(|(_, size)| *size).collect::<Vec<u32>>();
        sizes.sort();
        sizes
            .iter()
            .find_map(|size| {
                if size >= &need_to_free {
                    Some(size)
                } else {
                    None
                }
            })
            .unwrap()
            .to_string()
    }
}

fn calc_dir_sizes(memo: &mut HashMap<NodeId, u32>, node: NodeId, fs: &Arena<Output>) -> u32 {
    match memo.get(&node).map(|entry| entry.clone()) {
        Some(res) => res,
        None => {
            let children = node.children(&fs);
            let mut total = 0;
            for child in children {
                total += match fs.get(child).unwrap().get() {
                    Output::Dir { .. } => calc_dir_sizes(memo, child, fs).to_owned(),
                    Output::File { size, .. } => size.to_owned(),
                }
            }
            memo.insert(node, total);
            total
        }
    }
}

#[derive(Debug)]
enum Command {
    Cd { dir: String },
    Ls { children: Vec<Output> },
}

#[derive(Debug, Clone)]
enum Output {
    Dir { name: String },
    File { name: String, size: u32 },
}

fn parse_cd(data: &str) -> IResult<&str, Command> {
    map(
        preceded(
            ws(char('$')),
            preceded(ws(tag("cd")), alt((tag("/"), tag(".."), alpha1))),
        ),
        |val| Command::Cd {
            dir: val.to_owned(),
        },
    )(data)
}

fn parse_ls_dir(data: &str) -> IResult<&str, Output> {
    map(preceded(ws(tag("dir")), alpha1), |val| Output::Dir {
        name: val.to_owned(),
    })(data)
}

fn parse_ls_file(data: &str) -> IResult<&str, Output> {
    map(tuple((ws(decimal), not_line_ending)), |(size, name)| {
        Output::File {
            name: name.to_owned(),
            size: size.to_owned(),
        }
    })(data)
}

fn parse_ls(data: &str) -> IResult<&str, Command> {
    let (rest, _) = tuple((ws(tag("$")), ws(tag("ls"))))(data)?;
    let (rest, outputs) = separated_list1(newline, alt((parse_ls_dir, parse_ls_file)))(rest)?;
    Ok((rest, Command::Ls { children: outputs }))
}

fn parse_cmd(data: &str) -> IResult<&str, Command> {
    alt((parse_cd, parse_ls))(data)
}

fn generate_fs(commands: Vec<Command>) -> (NodeId, Arena<Output>) {
    let fs = &mut Arena::new();
    let root = fs.new_node(Output::Dir {
        name: String::from("/"),
    });
    let mut current = root;
    for command in commands.iter() {
        match command {
            Command::Cd { dir } => {
                if dir == ".." {
                    current = fs.get(current).unwrap().parent().unwrap();
                } else if dir == "/" {
                    current = root;
                } else {
                    let new_current = current
                        .children(fs)
                        .find(|child| match fs.get(child.to_owned()).unwrap().get() {
                            Output::Dir { name } => name == dir,
                            Output::File { name, .. } => name == dir,
                        })
                        .unwrap_or_else(|| panic!("Failed to find dir '{}'", dir));
                    current = new_current;
                }
            }
            Command::Ls { children } => {
                for child in children {
                    let new_node = fs.new_node(child.to_owned());
                    current.append(new_node, fs);
                }
            }
        };
    }
    (root, fs.to_owned())
}

fn load() -> (NodeId, Arena<Output>) {
    let data = problem::load(7);
    let (_, res) = separated_list1(newline, parse_cmd)(data.as_str()).unwrap();
    generate_fs(res)
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
