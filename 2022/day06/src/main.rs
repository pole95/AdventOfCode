use std::{collections::HashSet, fs, time::Instant};

fn part1(input: &str) -> usize {
    let chars: Vec<char> = input.chars().collect();
    find_marker(&chars, 4)
}

fn part2(input: &str) -> usize {
    let chars: Vec<char> = input.chars().collect();
    find_marker(&chars, 14)
}

fn find_marker(chars: &[char], marker_len: usize) -> usize {
    match chars.windows(marker_len).enumerate().find(|(_, ch)| {
        let s: HashSet<char> = ch.iter().copied().collect();
        s.len() == marker_len
    }) {
        Some((i, _)) => i + marker_len,
        _ => unreachable!("No Marker"),
    }
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
