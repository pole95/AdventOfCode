use std::{fs, time::Instant};

struct Grid {
    data: Vec<u32>,
    width: usize,
    height: usize,
}

impl Grid {
    fn new(input: &str) -> Self {
        let width = input.split_once("\n").unwrap().0.len();
        let height = input.lines().count();
        let grid = input
            .lines()
            .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()))
            .flatten()
            .collect();
        Self {
            data: grid,
            width,
            height,
        }
    }

    fn get(self: &Grid, x: usize, y: usize) -> u32 {
        self.data[y * self.width + x]
    }
}

fn is_visible(grid: &Grid, x: usize, y: usize) -> bool {
    let curr_height = grid.get(x, y);

    (0..x)
        .filter(|curr_x| grid.get(*curr_x, y) >= curr_height)
        .count()
        == 0
        || (0..y)
            .filter(|curr_y| grid.get(x, *curr_y) >= curr_height)
            .count()
            == 0
        || (x + 1..grid.width)
            .filter(|curr_x| grid.get(*curr_x, y) >= curr_height)
            .count()
            == 0
        || (y + 1..grid.height)
            .filter(|curr_y| grid.get(x, *curr_y) >= curr_height)
            .count()
            == 0
}

fn scenic_score(grid: &Grid, x: usize, y: usize) -> usize {
    let curr_height = grid.get(x, y);

    let left = if let Some(p) = (0..x)
        .rev()
        .map(|curr_x| grid.get(curr_x, y) >= curr_height)
        .position(|v| v)
    {
        p + 1
    } else {
        x
    };

    let up = if let Some(p) = (0..y)
        .rev()
        .map(|curr_y| grid.get(x, curr_y) >= curr_height)
        .position(|v| v)
    {
        p + 1
    } else {
        y
    };
    let right = if let Some(p) = (x + 1..grid.width)
        .map(|curr_x| grid.get(curr_x, y) >= curr_height)
        .position(|v| v)
    {
        p + 1
    } else {
        grid.width - x - 1
    };

    let down = if let Some(p) = (y + 1..grid.height)
        .map(|curr_y| grid.get(x, curr_y) >= curr_height)
        .position(|v| v)
    {
        p + 1
    } else {
        grid.height - y - 1
    };

    left * right * up * down
}

fn part1(input: &Grid) -> usize {
    let mut count = (input.width + input.height) * 2 - 4;
    for y in 1..input.height - 1 {
        for x in 1..input.width - 1 {
            if is_visible(input, x, y) {
                count += 1;
            }
        }
    }
    count
}

fn part2(input: &Grid) -> usize {
    let mut score = 0;
    for y in 1..input.height - 1 {
        for x in 1..input.width - 1 {
            let newscore = scenic_score(input, x, y);
            if newscore > score {
                score = newscore;
            }
        }
    }
    score
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let grid = Grid::new(&input);

    let start = Instant::now();
    let p1 = part1(&grid);
    let end_part1 = Instant::now();

    let p2 = part2(&grid);
    let end_part2 = Instant::now();
    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    println!("Part1: {}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());

    println!("Part2: {}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
