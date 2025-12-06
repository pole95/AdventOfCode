use ndarray::{Array2, array};
use ndarray_conv::{ConvExt, ConvMode, PaddingMode};
use std::{fs, time::Instant};

fn solve(input: &str) -> (u64, u64) {
    let rows: Vec<Vec<f32>> = fs::read_to_string(input)
        .unwrap()
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| match c {
                    '@' => 1.0,
                    _ => 0.0,
                })
                .collect()
        })
        .collect();
    let h = rows.len();
    let w = rows[0].len();
    let flattened = rows.into_iter().flatten().collect();

    let mut array = Array2::from_shape_vec((h, w), flattened).unwrap();
    let kernel: Array2<f32> = array![[1.0, 1.0, 1.0], [1.0, 0.0, 1.0], [1.0, 1.0, 1.0]];
    let mut conv = array
        .conv(&kernel, ConvMode::Same, PaddingMode::Zeros)
        .unwrap();
    let mut removable = conv.mapv(|x| if x < 4.0 { 1.0 } else { 0.0 });
    let mut res = &array * &removable;
    let p1 = res.sum() as u64;
    let mut p2 = p1;
    while res.sum() > 0.0 {
        array = array - res;
        conv = array
            .conv(&kernel, ConvMode::Same, PaddingMode::Zeros)
            .unwrap();
        removable = conv.mapv(|x| if x < 4.0 { 1.0 } else { 0.0 });
        res = &array * &removable;
        p2 += res.sum() as u64;
    }

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
