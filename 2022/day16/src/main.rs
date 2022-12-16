use std::{collections::HashMap, fs, time::Instant};

#[derive(Debug)]
struct Valve {
    name: String,
    flow: u32,
    edges: Vec<usize>,
    mask: u32,
}

fn generate_empty_state(rooms: usize, valves: usize) -> Vec<Vec<i32>> {
    vec![vec![-1 as i32; 1 << valves]; rooms]
}

fn part1(input: &Vec<Valve>, nvalves: &usize) -> i32 {
    let get_new_state = || generate_empty_state(input.len(), *nvalves);
    let mut state = get_new_state();
    state[input.iter().position(|v| v.name == "AA").unwrap()][0] = 0;

    for m in 0..30 {
        state_update(&mut state, m, input, &get_new_state);
    }
    *state.iter().flatten().max().unwrap()
}

fn state_update(
    state: &mut Vec<Vec<i32>>,
    m: u32,
    input: &Vec<Valve>,
    new_state_fun: &dyn Fn() -> Vec<Vec<i32>>,
) {
    let mut new_state = new_state_fun();
    for i in 0..state.len() {
        let total_flow = (29 - m) * input[i].flow;
        for j in 0..state[i].len() {
            if state[i][j] >= 0 {
                if total_flow > 0 && (input[i].mask as usize & j) == 0 {
                    let new_mask = input[i].mask as usize | j;
                    new_state[i][new_mask] =
                        new_state[i][new_mask].max(state[i][j] + total_flow as i32);
                }

                for edge in &input[i].edges {
                    new_state[*edge][j] = new_state[*edge][j].max(state[i][j]);
                }
            }
        }
    }
    *state = new_state;
}

fn part2(input: &Vec<Valve>, nvalves: &usize) -> i32 {
    let get_new_state = || generate_empty_state(input.len(), *nvalves);
    let mut state = get_new_state();
    let start_idx = input.iter().position(|v| v.name == "AA").unwrap();
    state[start_idx][0] = 0;
    for player in 0..2 {
        for m in 4..30 {
            state_update(&mut state, m, input, &get_new_state);
        }

        if player == 0 {
            let mut new_state = get_new_state();
            for i in 0..state.len() {
                for j in 0..state[i].len() {
                    new_state[start_idx][j] = new_state[start_idx][j].max(state[i][j]);
                }
            }
            state = new_state;
        }
    }
    *state.iter().flatten().max().unwrap()
}

fn parse_input(input: String) -> (Vec<Valve>, usize) {
    let mut valveidx = HashMap::<String, usize>::with_capacity(60);
    let mut tmp = vec![];
    let mut valves = Vec::<Valve>::with_capacity(60);
    for l in input.lines() {
        let splits = l.split(' ').collect::<Vec<_>>();
        let name = splits[1].to_string();

        let flow = splits[4]
            .split_once("=")
            .unwrap()
            .1
            .trim_end_matches(';')
            .parse::<u32>()
            .unwrap();
        let edgejoins = splits[9..].join("").to_string();
        let edgenames = edgejoins
            .split(',')
            .map(|s| s.to_string())
            .collect::<Vec<_>>();
        tmp.push((name.clone(), flow, edgenames));
        valveidx.insert(name, tmp.len() - 1);
    }
    let mut nvalves = 0;
    for (name, flow, edgenames) in tmp {
        let edges = edgenames.iter().map(|e| valveidx[e]).collect::<Vec<_>>();
        let mut mask = 0;
        if flow > 0 {
            mask = 1 << nvalves;
            nvalves += 1;
        }
        valves.push(Valve {
            name: name.to_string(),
            flow,
            edges,
            mask,
        });
    }

    (valves, nvalves)
}
fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let (valves, nvalves) = parse_input(input);
    let start = Instant::now();
    let p1 = part1(&valves, &nvalves);
    let end_part1 = Instant::now();
    let p2 = part2(&valves, &nvalves);
    let end_part2 = Instant::now();

    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    println!("Part1: {:?}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());

    println!("Part2: {:?}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
