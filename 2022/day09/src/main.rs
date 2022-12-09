use num::{complex::Complex, integer::Roots};
use std::{
    collections::{HashMap, HashSet},
    fs,
    time::Instant,
};

fn simulate_rope(input: &str, n_knots: usize) -> usize {
    let mut rope = vec![Complex::new(0, 0); n_knots];
    let moves = HashMap::from([
        ("L", Complex::new(-1, 0)),
        ("R", Complex::new(1, 0)),
        ("U", Complex::new(0, 1)),
        ("D", Complex::new(0, -1)),
    ]);
    let mut hist: HashSet<Complex<i32>> = HashSet::new();
    hist.insert(rope[0]);
    for l in input.lines() {
        let (m, n) = l.split_once(" ").unwrap();
        for _ in 0..n.parse().unwrap() {
            rope[0] += moves[m];
            for k in 1..n_knots {
                let d = rope[k - 1] - rope[k];
                if d.norm_sqr().sqrt() >= 2 {
                    rope[k] = rope[k - 1] - d / 2;
                }
            }
            hist.insert(*rope.last().unwrap());
        }
    }
    hist.len()
}

fn part1(input: &str) -> usize {
    simulate_rope(input, 2)
}

fn part2(input: &str) -> usize {
    simulate_rope(input, 10)
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
