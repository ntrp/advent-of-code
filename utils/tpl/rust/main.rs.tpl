use std::time::Instant;

use clap::{Parser, Subcommand};
use days::ALL;

mod days;
mod problem;
mod utils;
mod parsers;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Run a solution to a problem")]
    Solve { day: usize, part: char },
    #[command(about = "List all solutions for a given year")]
    List {},
}

fn main() {
    let args = Cli::parse();
    match args.command {
        Commands::Solve { day, part } => run(day, part),
        Commands::List {} => {
            println!("[*] Solutions for <<YEAR>>:");

            for (i, e) in ALL.iter().enumerate() {
                println!(
                    " {} Day {}: {}",
                    if i + 1 == ALL.len() { "└" } else { "├" },
                    i + 1,
                    e.name()
                );
            }
        }
    }
}

fn run(day: usize, part: char) {
    let solution = match ALL.get(day.saturating_sub(1) as usize) {
        Some(s) => s,
        None => {
            println!("No solution for day {}", day);
            return;
        }
    };
    println!("[*] Running: {} ({})", solution.name(), part.to_uppercase());

    let start = Instant::now();
    let out = match part.to_lowercase().to_string().as_str() {
        "a" => solution.part_a(),
        "b" => solution.part_b(),
        _ => return println!("[-] Invalid Part {}", part),
    };

    let time = start.elapsed().as_nanos();
    println!("[+] OUT: {} ({})", out, utils::time_unit(time));
}
