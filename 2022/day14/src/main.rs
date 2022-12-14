use std::{fs, time::Instant};

use itertools::Itertools;
#[derive(Clone)]
struct Map {
    data: Vec<bool>,
    sand_point: (usize, usize),
    width: usize,
    height: usize,
}

impl Map {
    fn new(input: &str) -> Self {
        let mut data = vec![false; 1000 * 1000 as usize];
        let mut max_y = 0;
        for line in input.lines() {
            for (start, end) in line
                .split(" -> ")
                .map(|c| c.split(","))
                .map(|mut s| {
                    (
                        s.next().unwrap().parse::<usize>().unwrap(),
                        s.next().unwrap().parse::<usize>().unwrap(),
                    )
                })
                .tuple_windows()
            {
                for y in start.1.min(end.1)..=start.1.max(end.1) {
                    for x in start.0.min(end.0)..=start.0.max(end.0) {
                        data[1000 * y + x] = true;
                        max_y = std::cmp::max(max_y, y);
                    }
                }
            }
        }

        let sand_point = (500, 0);

        Self {
            data,
            sand_point,
            width: 1000,
            height: max_y as usize,
        }
    }
    fn set(&mut self, coord: &(usize, usize)) {
        self.data[self.width * coord.1 + coord.0] = true;
    }
    fn get(&self, coord: &(usize, usize)) -> bool {
        *self
            .data
            .get(self.width * coord.1 + coord.0)
            .unwrap_or_else(|| panic!("Coordinate out of bounds"))
    }
    fn simulate_sand(&mut self, has_floor: bool) -> u32 {
        let mut c: u32 = 0;
        while let Some(loc) = self.simulate_grain(has_floor) {
            self.set(&loc);
            c += 1
        }
        c
    }
    fn simulate_grain(&self, has_floor: bool) -> Option<(usize, usize)> {
        let floor = self.height + 2;
        let mut grain = self.sand_point;
        if self.get(&grain) {
            return None;
        }
        while !(self.get(&(grain.0, grain.1 + 1))
            && self.get(&(grain.0 + 1, grain.1 + 1))
            && self.get(&(grain.0 - 1, grain.1 + 1)))
        {
            if grain.1 == floor - 1 {
                if has_floor {
                    break;
                } else {
                    return None;
                }
            } else if !self.get(&(grain.0, grain.1 + 1)) {
                grain.1 += 1;
            } else if !self.get(&(grain.0 - 1, grain.1 + 1)) {
                grain.0 -= 1;
                grain.1 += 1;
            } else {
                grain.0 += 1;
                grain.1 += 1;
            }
        }

        Some(grain)
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let start = Instant::now();
    let rocks = Map::new(&input);

    let p1 = part1(rocks.clone());
    let end_part1 = Instant::now();

    let p2 = part2(rocks);
    let end_part2 = Instant::now();
    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    println!("Part1: {:?}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());

    println!("Part2: {:?}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}

fn part1(mut input: Map) -> u32 {
    input.simulate_sand(false)
}
fn part2(mut input: Map) -> u32 {
    input.simulate_sand(true)
}
