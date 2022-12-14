use std::{cmp::Ordering, fs, time::Instant};

use serde_json::{json, Value};

fn part1(input: &Vec<Value>) -> usize {
    let pairs: Vec<Vec<Value>> = input.chunks(2).map(|p| p.to_vec()).collect();
    pairs
        .iter()
        .map(|p| check_pairs(&p[0], &p[1]))
        .enumerate()
        .filter(|(_, p)| p.is_some() && matches!(p.unwrap(), Ordering::Less))
        .map(|(i, _)| i + 1)
        .sum()
}

fn part2(mut input: Vec<Value>) -> usize {
    input.extend([json!([[2]]), json!([[6]])]);
    input.sort_by(|a, b| check_pairs(a, b).unwrap());
    input
        .iter()
        .enumerate()
        .filter(|(_, p)| **p == json!([[2]]) || **p == json!([[6]]))
        .map(|(i, _)| i + 1)
        .product()
}

fn check_pairs(a: &Value, b: &Value) -> Option<Ordering> {
    if let (Some(a), Some(b)) = (a.as_u64(), b.as_u64()) {
        match a.cmp(&b) {
            Ordering::Equal => None,
            o => Some(o),
        }
    } else if let (Some(a), Some(b)) = (a.as_array(), b.as_array()) {
        if a.is_empty() || b.is_empty() {
            match a.len().cmp(&b.len()) {
                Ordering::Equal => None,
                o => Some(o),
            }
        } else if let Some(v) = check_pairs(&a[0], &b[0]) {
            Some(v)
        } else {
            check_pairs(&json!(a[1..]), &json!(b[1..]))
        }
    } else if let (Some(a), Some(b)) = (a.as_u64(), b.as_array()) {
        check_pairs(&json!(vec![a]), &json!(b))
    } else if let (Some(a), Some(b)) = (a.as_array(), b.as_u64()) {
        check_pairs(&json!(a), &json!(vec![b]))
    } else {
        Some(Ordering::Greater)
    }
}
fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let parsed: Vec<Value> = input
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| serde_json::from_str(l).unwrap())
        .collect();
    let start = Instant::now();
    let p1 = part1(&parsed);
    let end_part1 = Instant::now();

    let p2 = part2(parsed);
    let end_part2 = Instant::now();
    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    println!("Part1: {:?}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());

    println!("Part2: {:?}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
