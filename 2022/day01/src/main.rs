use itertools::Itertools;
use std::{fs, time::Instant};

fn part1(input: &str) -> i32 {
    fs::read_to_string(input)
        .unwrap()
        .split("\n\n")
        .map(|x| x.lines().filter_map(|x| x.parse::<i32>().ok()).sum())
        .max()
        .unwrap_or(0)
}

fn part2(input: &str) -> i32 {
    fs::read_to_string(input)
        .unwrap()
        .split("\n\n")
        .map(|x| x.lines().filter_map(|x| x.parse::<i32>().ok()).sum())
        .sorted_by(|a: &i32, b: &i32| b.cmp(a))
        .take(3)
        .sum::<i32>()
}

fn main() {
    let start = Instant::now();
    let p1 = part1("input.txt");
    let end_part1 = Instant::now();
    let part1_time = end_part1 - start;
    println!("Part1: {}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());
    let p2 = part2("input.txt");
    let end_part2 = Instant::now();
    let part2_time = end_part2 - start;
    println!("Part2: {}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
