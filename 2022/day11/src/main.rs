use std::{fs, time::Instant};

struct Monkey {
    items: Vec<usize>,
    inspected: usize,
    divisor: usize,
    test: Box<dyn Fn(usize) -> usize>,
    operation: Box<dyn Fn(usize) -> usize>,
}

impl Monkey {
    fn new(input: &str) -> Self {
        let lines: Vec<&str> = input.lines().collect();
        let items = lines[1]
            .strip_prefix("  Starting items: ")
            .unwrap()
            .split(",")
            .map(|x| x.trim().parse().unwrap())
            .collect::<Vec<usize>>();

        let divisor = lines[3]
            .strip_prefix("  Test: divisible by ")
            .unwrap()
            .parse::<usize>()
            .unwrap();
        let monkey_true = lines[4]
            .strip_prefix("    If true: throw to monkey ")
            .unwrap()
            .parse::<usize>()
            .unwrap();
        let monkey_false = lines[5]
            .strip_prefix("    If false: throw to monkey ")
            .unwrap()
            .parse::<usize>()
            .unwrap();

        Self {
            items,
            inspected: 0,
            divisor,
            test: Box::new(move |x| {
                if x % divisor == 0 {
                    monkey_true
                } else {
                    monkey_false
                }
            }),
            operation: parse_operation(lines[2]).unwrap(),
        }
    }
}

fn parse_operation(line: &str) -> Option<Box<dyn Fn(usize) -> usize>> {
    let op = line.strip_prefix("  Operation: new = old ").unwrap();
    if let Some(n) = op.strip_prefix("+ ") {
        if n == "old" {
            Some(Box::new(move |x| x + x))
        } else {
            let n = n.parse::<usize>().unwrap();
            Some(Box::new(move |x| x + n))
        }
    } else if let Some(n) = op.strip_prefix("* ") {
        if n == "old" {
            Some(Box::new(move |x| x * x))
        } else {
            let n = n.parse::<usize>().unwrap();
            Some(Box::new(move |x| x * n))
        }
    } else {
        None
    }
}
fn part1(mut input: Vec<Monkey>) -> usize {
    for _ in 0..20 {
        for i in 0..input.len() {
            while input[i].items.len() > 0 {
                input[i].inspected += 1;
                let new = (input[i].operation)(input[i].items[0]) / 3;
                let ni = (input[i].test)(new);
                input[ni].items.push(new);
                input[i].items.remove(0);
            }
        }
    }
    input.sort_by_key(|x| x.inspected);
    input.reverse();
    input[0].inspected * input[1].inspected
}

fn part2(mut input: Vec<Monkey>) -> usize {
    let modulo: usize = input.iter().map(|x| x.divisor).product();
    for _ in 0..10000 {
        for i in 0..input.len() {
            while input[i].items.len() > 0 {
                input[i].inspected += 1;
                let new = (input[i].operation)(input[i].items[0]) % modulo;
                let ni = (input[i].test)(new);
                input[ni].items.push(new);
                input[i].items.remove(0);
            }
        }
    }
    input.sort_by_key(|x| x.inspected);
    input.reverse();
    input[0].inspected * input[1].inspected
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut monkeys = Vec::with_capacity(10);
    let mut monkeys2 = Vec::with_capacity(10);

    for m in input.split("\n\n") {
        monkeys.push(Monkey::new(m));
        monkeys2.push(Monkey::new(m));
    }

    let start = Instant::now();
    let p1 = part1(monkeys);
    let end_part1 = Instant::now();

    let p2 = part2(monkeys2);
    let end_part2 = Instant::now();
    let part1_time = end_part1 - start;
    let part2_time = end_part2 - end_part1;

    println!("Part1: {}", p1);
    println!("Part1 Duration: {} µs", part1_time.as_micros());

    println!("Part2: {}", p2);
    println!("Part2 Duration: {} µs", part2_time.as_micros());
}
