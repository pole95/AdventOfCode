use std::{fs, time::Instant};

#[derive(Debug, Clone, Copy)]
struct Range {
    low: u64,
    high: u64,
}
fn solve(input: &str) -> (u64, u64) {
    fs::read_to_string(input)
        .unwrap()
        .split(",")
        .map(|s| {
            let (lo, hi) = s.split_once('-').unwrap();
            Range {
                low: lo.parse::<u64>().unwrap(),
                high: hi.parse::<u64>().unwrap(),
            }
        })
        .fold((0, 0), |(mut sol1, mut sol2), r| {
            for id in r.low..=r.high {
                if invalid1(id) {
                    sol1 += id;
                }
                if invalid2(id) {
                    sol2 += id;
                }
            }
            (sol1, sol2)
        })
}

fn invalid1(id: u64) -> bool {
    let num_digits = id.max(1).ilog10() + 1;
    if num_digits.is_multiple_of(2) {
        let half_magnitude = 10_u64.pow(num_digits / 2);
        id % half_magnitude == id / half_magnitude
    } else {
        false
    }
}

fn invalid2(id: u64) -> bool {
    let num_digits = id.max(1).ilog10() + 1;
    for d in 1..=num_digits / 2 {
        if !num_digits.is_multiple_of(d) {
            continue;
        }
        let k = num_digits / d;
        let pattern = (10_u64.pow(k * d) - 1) / (10_u64.pow(d) - 1);
        if id.is_multiple_of(pattern) {
            return true;
        }
    }
    false
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
