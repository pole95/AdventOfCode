use std::{fs, time::Instant};

fn solve(input: &str) -> (i32, i32) {
    let binding = fs::read_to_string(input).unwrap();
    let (_, sol1, sol2) = binding
        .lines()
        .map(|line| {
            let (dir, cnt) = line.split_at(1);
            if dir == "L" {
                -cnt.parse::<i32>().unwrap()
            } else {
                cnt.parse::<i32>().unwrap()
            }
        })
        .fold((50, 0, 0), |(acc, cnt, cnt2), n| {
            let new = (((acc + n) % 100) + 100) % 100;
            let new2 = acc + n % 100;
            let cross = (acc != 0 && new2 < 0) || new2 > 100;
            (
                new,
                cnt + if new == 0 { 1 } else { 0 },
                cnt2 + (n / 100).abs() + if cross { 1 } else { 0 },
            )
        });
    (sol1, sol1 + sol2)
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
