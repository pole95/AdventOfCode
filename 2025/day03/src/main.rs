use std::{fs, time::Instant};

fn solve(input: &str) -> (u64, u64) {
    let banks = fs::read_to_string(input)
        .unwrap()
        .lines()
        .fold((0, 0), |(mut p1, mut p2), l| {
            p1 += get_max_joltage(l.as_bytes(), 2);
            p2 += get_max_joltage(l.as_bytes(), 12);
            (p1, p2)
        });
    banks
}

fn get_max_joltage(mut bank: &[u8], num_batt: usize) -> u64 {
    let mut joltage = 0;
    for i in (0..num_batt).rev() {
        let (idx, n) = bank
            .iter()
            .enumerate()
            .rev()
            .skip(i)
            .max_by_key(|x| x.1)
            .unwrap();
        bank = &bank[idx + 1..];
        joltage = joltage * 10 + (n - b'0') as u64;
    }

    joltage
}

fn main() {
    let s = Instant::now();
    let sol = solve("input.txt");
    let e = Instant::now();
    let dur = e - s;

    println!("Part 1: {}", sol.0);
    println!("Part 2: {}", sol.1);
    println!("Duration: {} µs", dur.as_micros())
}
