use std::{fs, ops::Neg, time::Instant};

#[derive(Clone, Copy)]
struct Rock {
    data: [u8; 4],
}

impl Rock {
    fn new(shape: [u8; 4]) -> Self {
        Self { data: shape }
    }
    fn jet_move(&mut self, stream: &JetStream) {
        let left = match stream {
            JetStream::Left => true,
            _ => false,
        };
        self.data
            .iter_mut()
            .for_each(|r| if left { *r <<= 1 } else { *r >>= 1 })
    }
    fn hits_wall(&self, stream: &JetStream) -> bool {
        let bbox = self.data[0] | self.data[1] | self.data[2] | self.data[3];
        match stream {
            JetStream::Left => bbox & (1 << 7) != 0,
            JetStream::Right => bbox & (1 << 1) != 0,
        }
    }
}

struct Cave {
    data: Box<[u8]>,
    height: usize,
}

impl Cave {
    fn new() -> Self {
        Self {
            data: Box::new([0u8; 100_000]),
            height: 0,
        }
    }
    fn overlaps(&self, piece: &Rock, position: usize) -> bool {
        (piece.data[0] & self.data[position]
            | piece.data[1] & self.data[position + 1]
            | piece.data[2] & self.data[position + 2]
            | piece.data[3] & self.data[position + 3])
            != 0
    }

    fn anchor_rock(&mut self, piece: &Rock, position: usize) {
        self.data[position] |= piece.data[0];
        self.data[position + 1] |= piece.data[1];
        self.data[position + 2] |= piece.data[2];
        self.data[position + 3] |= piece.data[3];
    }

    fn get_height(&mut self) -> usize {
        while self.data[self.height] != 0 {
            self.height += 1;
        }
        self.height -= 1;

        self.height
    }
}

#[derive(Clone, Copy)]
enum JetStream {
    Left,
    Right,
}

impl Neg for JetStream {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            JetStream::Left => JetStream::Right,
            JetStream::Right => JetStream::Left,
        }
    }
}

fn part1(stream: &[JetStream], rock_bag: &[Rock; 5]) -> usize {
    let tgt = 2022;
    let mut tick = 0;
    let mut fallen_rocks = 1;
    let mut cave = Cave::new();
    let mut falling = rock_bag[0].clone();
    let mut vertical_pos = 3;
    while fallen_rocks < tgt {
        if !falling.hits_wall(&stream[tick % stream.len()]) {
            falling.jet_move(&stream[tick % stream.len()]);
        }
        if vertical_pos == 0 || cave.overlaps(&falling, vertical_pos - 1) {
            cave.anchor_rock(&falling, vertical_pos);
            let new_height = cave.get_height();
            falling = rock_bag[fallen_rocks % 5].clone();
            fallen_rocks += 1;
            vertical_pos = new_height + 4;
            /*println!("Tick {}:", tick);
            (0..=vertical_pos + 3)
                .rev()
                .for_each(|i| println!("{:08b}", cave.data[i]));*/
        } else {
            vertical_pos -= 1;
        }

        tick += 1;
    }

    cave.get_height() + 1
}

fn part2(stream: &[JetStream]) -> usize {
    0
}

fn main() {
    let rocks: [Rock; 5] = [
        Rock::new([0b00111100, 0b00000000, 0b00000000, 0b00000000]),
        Rock::new([0b00010000, 0b00111000, 0b00010000, 0b00000000]),
        Rock::new([0b00111000, 0b00001000, 0b00001000, 0b00000000]),
        Rock::new([0b00100000, 0b00100000, 0b00100000, 0b00100000]),
        Rock::new([0b00110000, 0b00110000, 0b00000000, 0b00000000]),
    ];

    let input = fs::read_to_string("input.txt").unwrap();
    let stream = input
        .trim()
        .chars()
        .map(|c| match c {
            '<' => JetStream::Left,
            '>' => JetStream::Right,
            _ => panic!("Unkown char in input: {}", c),
        })
        .collect::<Vec<_>>();

    let start = Instant::now();
    let p1 = part1(&stream, &rocks);
    let end_part1 = Instant::now();
    let p2 = part2(&stream);
    let end_part2 = Instant::now();

    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    println!("Part1: {:?}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());

    println!("Part2: {:?}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
