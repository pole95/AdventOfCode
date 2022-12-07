use std::{fs, time::Instant};

fn get_all_dir_sizes(input: &str) -> Vec<u32> {
    let mut all_dirs: Vec<u32> = Vec::new();
    let mut current_dirs: Vec<u32> = Vec::new();
    for l in input.lines() {
        if l == "$ cd .." {
            all_dirs.push(current_dirs.pop().unwrap());
        } else if l.starts_with("$ cd") {
            current_dirs.push(0);
        } else if l != "$ ls" && !l.starts_with("dir") {
            let (a, _) = l.split_once(" ").unwrap();
            let size = a.parse::<u32>().unwrap();
            for i in &mut current_dirs {
                *i += size;
            }
        }
    }
    all_dirs.extend(current_dirs);
    all_dirs
}

fn part1(input: &str) -> u32 {
    let all_dirs = get_all_dir_sizes(input);
    all_dirs.iter().filter(|s| **s <= 100000).sum()
}

fn part2(input: &str) -> u32 {
    let all_dirs = get_all_dir_sizes(input);
    let free_space = 70000000 - all_dirs.iter().max().unwrap();
    let min_dir_size = 30000000 - free_space;
    *all_dirs
        .iter()
        .filter(|s| **s >= min_dir_size)
        .min()
        .unwrap()
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
