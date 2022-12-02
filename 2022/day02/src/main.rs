use std::{fs, time::Instant};

fn score1(inp: &str) -> i32 {
    match inp {
        "A X" => 4,
        "A Y" => 8,
        "A Z" => 3,
        "B X" => 1,
        "B Y" => 5,
        "B Z" => 9,
        "C X" => 7,
        "C Y" => 2,
        "C Z" => 6,
        _ => {
            println!("UNKNOWN {}", inp);
            0
        }
    }
}

fn score2(inp: &str) -> i32 {
    match inp {
        "A X" => 3,
        "A Y" => 4,
        "A Z" => 8,
        "B X" => 1,
        "B Y" => 5,
        "B Z" => 9,
        "C X" => 2,
        "C Y" => 6,
        "C Z" => 7,
        _ => {
            println!("UNKNOWN {}", inp);
            0
        }
    }
}

fn part1(input: &str) -> i32 {
    fs::read_to_string(input).unwrap().lines().map(score1).sum()
}
fn part2(input: &str) -> i32 {
    fs::read_to_string(input).unwrap().lines().map(score2).sum()
}

fn main() {
    let start = Instant::now();
    let p1 = part1("input.txt");
    let end_part1 = Instant::now();
    let p2 = part2("input.txt");
    let end_part2 = Instant::now();
    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;
    println!("Part1: {}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());
    println!("Part2: {}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
