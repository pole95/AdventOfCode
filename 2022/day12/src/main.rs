use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap, HashSet},
    fs,
    str::FromStr,
    time::Instant,
};

struct HeightMap {
    start: (usize, usize),
    end: (usize, usize),
    map: HashMap<(usize, usize), u32>,
}

#[derive(Clone, Copy)]
struct Cell(usize, usize, u32);

#[derive(Debug)]
struct ParseMapError;

impl PartialEq for Cell {
    fn eq(&self, other: &Self) -> bool {
        self.2 == other.2
    }
}

impl PartialOrd for Cell {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Reverse(self.2).partial_cmp(&Reverse(other.2))
    }
}

impl Eq for Cell {}
impl Ord for Cell {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Reverse(self.2).cmp(&Reverse(other.2))
    }
}

impl FromStr for HeightMap {
    type Err = ParseMapError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut start = None;
        let mut end = None;
        let mut heights = HashMap::new();
        for (y, l) in s.lines().enumerate() {
            for (x, b) in l.bytes().enumerate() {
                match b {
                    b'S' => {
                        start = Some((x, y));
                        heights.insert((x, y), 0);
                    }
                    b'E' => {
                        end = Some((x, y));
                        heights.insert((x, y), 25);
                    }
                    byte => {
                        heights.insert((x, y), (byte - b'a') as u32);
                    }
                }
            }
        }
        let start = match start {
            Some(s) => s,
            None => return Err(ParseMapError),
        };
        let end = match end {
            Some(e) => e,
            None => return Err(ParseMapError),
        };

        Ok(Self {
            start,
            end,
            map: heights,
        })
    }
}

impl HeightMap {
    fn neighbours(x: usize, y: usize) -> Vec<(usize, usize)> {
        [
            (x as isize + 1, y as isize),
            (x as isize - 1, y as isize),
            (x as isize, y as isize + 1),
            (x as isize, y as isize - 1),
        ]
        .into_iter()
        .filter(|(x, y)| *x >= 0 && *y >= 0)
        .map(|(x, y)| (x as usize, y as usize))
        .collect()
    }

    fn a_star(&self, start: (usize, usize)) -> Option<u32> {
        let mut open_set = BinaryHeap::from([Cell(start.0, start.1, 0)]);
        let mut visited = HashSet::new();
        visited.insert(start);

        while !open_set.is_empty() {
            let Cell(x, y, cost) = open_set.pop().unwrap();
            if (x, y) == self.end {
                return Some(cost);
            }
            let curr_height = self.map.get(&(x, y)).unwrap();
            for (nx, ny) in Self::neighbours(x, y) {
                if !visited.contains(&(nx, ny)) {
                    if let Some(new_height) = self.map.get(&(nx, ny)) {
                        if curr_height + 1 >= *new_height {
                            visited.insert((nx, ny));
                            open_set.push(Cell(nx, ny, cost + 1));
                        }
                    }
                }
            }
        }
        None
    }
}

fn part1(input: &HeightMap) -> Option<u32> {
    input.a_star(input.start)
}

fn part2(input: &HeightMap) -> Option<u32> {
    input
        .map
        .iter()
        .filter(|(_, h)| **h == 0)
        .flat_map(|(s, _)| input.a_star(*s))
        .min()
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let map = HeightMap::from_str(&input).unwrap();
    let start = Instant::now();
    let p1 = part1(&map);
    let end_part1 = Instant::now();

    let p2 = part2(&map);
    let end_part2 = Instant::now();
    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    println!("Part1: {:?}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());

    println!("Part2: {:?}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
