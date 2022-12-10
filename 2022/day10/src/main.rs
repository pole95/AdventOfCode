use std::{fs, i32, time::Instant};

fn part1(input: &str) -> i32 {
    let mut x = 1;
    let mut x_hist = Vec::with_capacity(250);
    x_hist.push(x);
    for op in input.lines() {
        x_hist.push(x);
        if op.starts_with("a") {
            let (_, v) = op.split_once(' ').unwrap();
            x_hist.push(x);
            x += v.parse::<i32>().unwrap();
        }
    }
    x_hist
        .iter()
        .enumerate()
        .skip(20)
        .step_by(40)
        .take(6)
        .fold(0, |acc, (i, x)| acc + (i as i32 * x))
}

fn part2(input: &str) -> String {
    let mut x = 1;
    let mut crt = [32; 240];
    let mut cycle = 0;
    for op in input.lines() {
        draw(&x, &mut cycle, &mut crt);
        if op.starts_with("a") {
            let (_, v) = op.split_once(' ').unwrap();
            draw(&x, &mut cycle, &mut crt);
            x += v.parse::<i32>().unwrap();
        }
    }
    crt.chunks(40)
        .map(|l| std::str::from_utf8(l).unwrap())
        .fold(String::with_capacity(250), |mut s, l| {
            s.push('\n');
            s.push_str(l);
            s
        })
}

fn draw(x: &i32, cycle: &mut i32, crt: &mut [u8; 240]) {
    let should_draw = (x - 1..=x + 1).contains(&(*cycle % 40));
    if should_draw {
        crt[*cycle as usize] = 35;
    }
    *cycle += 1;
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
