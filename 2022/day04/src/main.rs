use itertools::Itertools;
use std::{fs, num::ParseIntError, str::FromStr, time::Instant};

#[derive(Clone, Copy)]
struct Range {
    min: u32,
    max: u32,
}

impl FromStr for Range {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = s.split_once("-").unwrap();
        let min = match a.parse::<u32>() {
            Ok(m) => m,
            Err(err) => return Err(err),
        };
        let max = match b.parse::<u32>() {
            Ok(m) => m,
            Err(err) => return Err(err),
        };

        Ok(Self { min, max })
    }
}

impl Range {
    fn contains(self, other: Self) -> bool {
        self.min <= other.min && self.max >= other.max
    }

    fn overlaps(self, other: Self) -> bool {
        self.min >= other.min && self.min <= other.max
            || self.max >= other.min && self.max <= other.max
    }
}

fn part1(input: &str) -> usize {
    input
        .lines()
        .filter(|l| {
            let (a, b) = l
                .split(",")
                .map(|x| Range::from_str(x).unwrap())
                .next_tuple()
                .unwrap();
            a.contains(b) || b.contains(a)
        })
        .count()
}

fn part2(input: &str) -> usize {
    input
        .lines()
        .filter(|l| {
            let (a, b) = l
                .split(",")
                .map(|x| Range::from_str(x).unwrap())
                .next_tuple()
                .unwrap();
            a.overlaps(b) || b.overlaps(a)
        })
        .count()
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let start = Instant::now();
    let p1 = part1(&input);
    let end_part1 = Instant::now();
    let p2 = part2(&input);
    let end_part2 = Instant::now();
    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    println!("Part1: {}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());

    println!("Part2: {}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
