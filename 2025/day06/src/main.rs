use std::{fs, time::Instant, u64};

fn solve(input: &str) -> (u64, u64) {
    let inp = fs::read_to_string(input).unwrap();
    let mut lines: Vec<_> = inp.lines().collect();
    let operators: Vec<_> = lines.pop().unwrap().split_whitespace().collect();

    let nums: Vec<Vec<&str>> = lines
        .iter()
        .map(|l| l.split_whitespace().collect())
        .collect();
    let p1: u64 = operators
        .iter()
        .enumerate()
        .map(|(i, op)| {
            let num = nums.iter().map(|row| row[i].parse::<u64>().unwrap());
            match *op {
                "*" => num.product::<u64>(),
                "+" => num.sum::<u64>(),
                _ => panic!("unknown operator!"),
            }
        })
        .sum();

    let num_matrix: Vec<Vec<char>> = lines.iter().map(|l| l.chars().collect()).collect();
    let transposed_matrix = transpose_numbers(num_matrix);
    let nums_tmp: Vec<String> = transposed_matrix
        .iter()
        .map(|l| l.iter().collect::<String>())
        .collect();

    let nums2: Vec<Vec<String>> = split_on_empty(nums_tmp);

    let p2: u64 = operators
        .iter()
        .enumerate()
        .map(|(i, op)| {
            let num = nums2[i].iter().map(|n| n.trim().parse::<u64>().unwrap());
            match *op {
                "*" => num.product::<u64>(),
                "+" => num.sum::<u64>(),
                _ => panic!("unknown operator!"),
            }
        })
        .sum();
    (p1, p2)
}

fn split_on_empty(items: Vec<String>) -> Vec<Vec<String>> {
    let mut result = Vec::new();
    let mut current = Vec::new();

    for s in items {
        if s.trim().is_empty() {
            // separator found
            if !current.is_empty() {
                result.push(current);
                current = Vec::new();
            }
        } else {
            current.push(s);
        }
    }

    // push the last group if it exists
    if !current.is_empty() {
        result.push(current);
    }

    result
}

fn transpose_numbers(row: Vec<Vec<char>>) -> Vec<Vec<char>> {
    let len = row[0].len();
    let mut iters: Vec<_> = row.into_iter().map(|n| n.into_iter()).collect();
    (0..len)
        .map(|_| {
            iters
                .iter_mut()
                .map(|n| n.next().unwrap())
                .collect::<Vec<char>>()
        })
        .collect()
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
