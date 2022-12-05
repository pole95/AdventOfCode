use std::{
    collections::{HashMap, VecDeque},
    fs,
    str::FromStr,
    time::Instant,
};

use regex::Regex;

fn parse_stacks(in_stacks: &str) -> Vec<VecDeque<u8>> {
    let mut stackmap = HashMap::<usize, VecDeque<u8>>::new();
    for l in in_stacks.lines().map(|c| c.as_bytes()) {
        if l[1].is_ascii_digit() {
            break;
        }
        for (i, c) in l.chunks(4).enumerate() {
            if c[1].is_ascii_whitespace() {
                continue;
            } else {
                stackmap.entry(i).or_default().push_front(c[1]);
            }
        }
    }
    (0..stackmap.len())
        .filter_map(|i| stackmap.remove(&i))
        .collect()
}
#[derive(Debug, Clone, Copy)]
struct Move {
    n: usize,
    s: usize,
    e: usize,
}
#[derive(Debug, Clone)]
struct ParseMoveError;

impl FromStr for Move {
    type Err = ParseMoveError;

    fn from_str(st: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"(\d+)").unwrap();
        let matches: Vec<usize> = re
            .find_iter(st)
            .filter_map(|m| m.as_str().parse::<usize>().ok())
            .collect();
        if matches.len() == 3 {
            Ok(Move {
                n: matches[0],
                s: matches[1],
                e: matches[2],
            })
        } else {
            for m in matches.iter() {
                println!("{:#?}", m);
            }
            Err(ParseMoveError)
        }
    }
}

fn part1(mut stacks: Vec<VecDeque<u8>>, moves: &Vec<Move>) -> String {
    for m in moves {
        for _ in 0..m.n {
            let v = stacks[m.s - 1].pop_back().unwrap();
            stacks[m.e - 1].push_back(v);
        }
    }
    String::from_utf8(stacks.iter_mut().filter_map(|s| s.pop_back()).collect()).unwrap()
}

fn part2(mut stacks: Vec<VecDeque<u8>>, moves: &Vec<Move>) -> String {
    for m in moves {
        let mut buf = Vec::<u8>::new();
        for _ in 0..m.n {
            buf.push(stacks[m.s - 1].pop_back().unwrap());
        }
        for v in buf.iter().rev() {
            stacks[m.e - 1].push_back(*v);
        }
    }
    String::from_utf8(stacks.iter_mut().filter_map(|s| s.pop_back()).collect()).unwrap()
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut splits = input.split("\n\n");
    let init_stacks = parse_stacks(splits.next().unwrap());
    let moves: Vec<Move> = splits
        .next()
        .unwrap()
        .trim()
        .lines()
        .map(|s| Move::from_str(s))
        .collect::<Result<_, _>>()
        .unwrap();

    let stacks = init_stacks.to_vec();
    let start = Instant::now();
    let p1 = part1(stacks, &moves);
    let end_part1 = Instant::now();

    let p2 = part2(init_stacks, &moves);
    let end_part2 = Instant::now();
    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    println!("Part1: {}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());

    println!("Part2: {}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
