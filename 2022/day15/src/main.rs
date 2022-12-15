use std::{fs, time::Instant};

use itertools::Itertools;

#[derive(Clone, Copy)]
struct Sensor {
    sensor: (i64, i64),
    distance: i64,
}

impl Sensor {
    fn can_sense(&self, other: &(i64, i64)) -> bool {
        (self.sensor.0 - other.0).abs() + (self.sensor.1 - other.1).abs() <= self.distance
    }
    fn can_see_all(&self, min: &(i64, i64), max: &(i64, i64)) -> bool {
        let corners = [
            (min.0, min.1),
            (min.0, max.1),
            (max.0, min.1),
            (max.0, max.1),
        ];

        let max_dist = corners
            .iter()
            .map(|c| (c.0 - self.sensor.0).abs() + (c.1 - self.sensor.1).abs())
            .max()
            .unwrap();
        max_dist <= self.distance
    }
}

fn part1(input: &[Sensor], target_row: i64) -> i64 {
    let covered = input
        .iter()
        .map(|&pair| {
            (
                pair.sensor.0,
                pair.distance - (target_row - pair.sensor.1).abs(),
            )
        })
        .filter(|&(_, covers)| covers >= 0)
        .flat_map(|(x, covers)| [(x - covers, true), (x + covers + 1, false)])
        .sorted()
        .collect::<Vec<_>>();

    let mut acc = -1;
    let mut nested = 0;
    let mut old_x = 0;
    for &(x, covers) in &covered {
        if nested > 0 {
            acc += x - old_x;
        }

        nested += if covers { 1 } else { -1 };
        old_x = x;
    }
    acc
}

fn part2(input: &[Sensor]) -> i64 {
    let mut stack = vec![((0, 0), (4000000i64, 4000000))];
    while let Some((min, max)) = stack.pop() {
        if min == max {
            if input.iter().all(|s| !s.can_sense(&min)) {
                return min.0 * 4000000 + min.1;
            }
        } else {
            let mid: (i64, i64) = ((min.0 + max.0) / 2, (min.1 + max.1) / 2);
            let quads = [
                (min, mid),
                ((mid.0 + 1, min.1), (max.0, mid.1)),
                ((min.0, mid.1 + 1), (mid.0, max.1)),
                ((mid.0 + 1, mid.1 + 1), max),
            ];

            for quadrant in quads {
                if quadrant.0 .0 > quadrant.1 .0 || quadrant.0 .1 > quadrant.1 .1 {
                    continue;
                }
                if !input
                    .iter()
                    .any(|s| s.can_see_all(&quadrant.0, &quadrant.1))
                {
                    stack.push(quadrant);
                }
            }
        }
    }
    unreachable!()
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let sensors = input
        .lines()
        .map(|l| {
            l.split(|c: char| !c.is_digit(10) && c != '-')
                .filter_map(|n| n.parse::<i64>().ok())
                .collect_tuple()
                .map(|(x, y, dx, dy)| Sensor {
                    sensor: (x, y),
                    distance: (x - dx).abs() + (y - dy).abs(),
                })
                .unwrap()
        })
        .collect::<Vec<_>>();

    let start = Instant::now();
    let p1 = part1(&sensors, 2000000);
    let end_part1 = Instant::now();
    let p2 = part2(&sensors);
    let end_part2 = Instant::now();

    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    println!("Part1: {:?}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());

    println!("Part2: {:?}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
