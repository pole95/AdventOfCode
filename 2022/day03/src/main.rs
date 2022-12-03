use itertools::Itertools;
use std::{collections::HashSet, fs, time::Instant};

fn get_priority(c: &char) -> u8 {
    match c {
        'a'..='z' => *c as u8 - b'a' as u8 + 1,
        'A'..='Z' => *c as u8 - b'A' as u8 + 27,
        _ => 0,
    }
}

fn part1(input: &str) -> u32 {
    let txt = fs::read_to_string(input).unwrap();
    /*let mut sum: u32 = 0;
    for line in txt.lines() {
        let (s1, s2) = line.split_at(line.len() / 2);
        let s1_set: HashSet<char> = HashSet::from_iter(s1.chars());
        let s2_set: HashSet<char> = HashSet::from_iter(s2.chars());
        let inter = s1_set.intersection(&s2_set);
        for c in inter {
            sum += get_priority(c) as u32;
        }
    }
    sum*/
    txt.lines()
        .map(|l| l.chars().find(|&c| l.rfind(c).unwrap() >= l.len() / 2))
        .map(|c| get_priority(&c.unwrap()) as u32)
        .sum()
}

fn part2(input: &str) -> u32 {
    let txt = fs::read_to_string(input).unwrap();

    txt.lines()
        .tuples::<(&str, &str, &str)>()
        .map(|(a, b, c)| a.chars().find(|&ch| b.contains(ch) && c.contains(ch)))
        .map(|c| get_priority(&c.unwrap()) as u32)
        .sum()
}

fn part1_set(input: &str) -> u32 {
    let txt = fs::read_to_string(input).unwrap();
    let mut sum: u32 = 0;
    for line in txt.lines() {
        let (s1, s2) = line.split_at(line.len() / 2);
        let s1_set: HashSet<char> = HashSet::from_iter(s1.chars());
        let s2_set: HashSet<char> = HashSet::from_iter(s2.chars());
        let inter = s1_set.intersection(&s2_set);
        for c in inter {
            sum += get_priority(c) as u32;
        }
    }
    sum
}

fn part2_set(input: &str) -> u32 {
    let txt = fs::read_to_string(input).unwrap();
    let mut sum: u32 = 0;
    for (s1, s2, s3) in txt.lines().tuples::<(_, _, _)>() {
        let s1_set: HashSet<char> = HashSet::from_iter(s1.chars());
        let s2_set: HashSet<char> = HashSet::from_iter(s2.chars());
        let s3_set: HashSet<char> = HashSet::from_iter(s3.chars());
        let inter: HashSet<char> = s1_set.intersection(&s2_set).cloned().collect();
        for c in inter.intersection(&s3_set) {
            sum += get_priority(c) as u32;
        }
    }
    sum
}

fn main() {
    let start = Instant::now();
    let p1 = part1("input.txt");
    let end_part1 = Instant::now();
    let p2 = part2("input.txt");
    let end_part2 = Instant::now();
    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    let start_set = Instant::now();
    let _ = part1_set("input.txt");
    let end_part1_set = Instant::now();
    let _ = part2_set("input.txt");
    let end_part2_set = Instant::now();
    let part1_time_set = end_part1_set - start_set;
    let part2_time_set = end_part2_set - end_part1_set;

    println!("Part1: {}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());
    println!("Part1 Duration /w sets: {} µs", part1_time_set.as_micros());

    println!("Part2: {}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
    println!("Part2 Duration /w sets: {} µs", part2_time_set.as_micros());
}
