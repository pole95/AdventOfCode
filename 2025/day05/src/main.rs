use std::{fs, time::Instant};

#[derive(Debug, Clone, Copy)]
struct Range {
    low: u64,
    high: u64,
}

impl Range {
    fn within(&self, other: &u64) -> bool {
        return other >= &self.low && other <= &self.high;
    }
}

impl Ord for Range {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.low.cmp(&other.low)
    }
}

impl PartialOrd for Range {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.low.partial_cmp(&other.low) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.high.partial_cmp(&other.high)
    }
}

impl PartialEq for Range {
    fn eq(&self, other: &Self) -> bool {
        self.low == other.low && self.high == other.high
    }
}

impl Eq for Range {}

fn solve(input: &str) -> (u64, u64) {
    let inp = fs::read_to_string(input).unwrap();
    let (ranges_raw, ids) = inp.split_once("\n\n").unwrap();
    let mut ranges: Vec<Range> = ranges_raw
        .lines()
        .map(|l| l.split_once("-").unwrap())
        .map(|(lo, hi)| Range {
            low: lo.parse::<u64>().unwrap_or(0),
            high: hi.parse::<u64>().unwrap_or(0),
        })
        .collect();
    ranges.sort();

    let p1 = ids
        .lines()
        .map(|id| id.parse::<u64>().unwrap_or(0))
        .filter(|id| ranges.iter().any(|r| r.within(id)))
        .count() as u64;
    let mut idx = 0;
    while idx < ranges.len() - 1 {
        let range1 = ranges[idx];
        let range2 = ranges[idx + 1];
        if range1.high < range2.low {
            idx += 1;
        } else {
            ranges[idx] = Range {
                low: range1.low.min(range2.low),
                high: range1.high.max(range2.high),
            };
            ranges.remove(idx + 1);
        }
    }
    let p2: u64 = ranges.iter().map(|r| r.high - r.low + 1).sum();
    (p1, p2)
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
