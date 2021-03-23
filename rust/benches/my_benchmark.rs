use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 1,
        1 => 1,
        n => fibonacci(n-1) + fibonacci(n-2),
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(black_box(20))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

use std::time::{Duration, Instant};
const iterations : i64 = 1_000_000_000;
fn null_call(i:i64) -> i64 {
    i
}
fn shift_call(i:i64) -> i64 {
    i<<13>>16
}
fn shift2_call(i:i64) -> i64 {
    i<<13>>16<<3
}
fn mask_call(i:i64) -> i64 {
    i&0xfffffffffff8_i64
}
pub fn timing() {
    let instant = Instant::now();
    for n in 1..iterations {
        null_call(iterations);
    }
    let null_time = instant.elapsed();
    println!("null   : {:?}",null_time);
    let instant = Instant::now();
    for n in 1..iterations {
        shift_call(iterations);
    }
    let shift_time = instant.elapsed();
    println!("shift  : {:?} {:?}",shift_time,(shift_time-null_time));
    let instant = Instant::now();
    for n in 1..iterations {
        shift2_call(iterations);
    }
    let shift2_time = instant.elapsed();
    println!("shift2 : {:?} {:?}",shift2_time,(shift2_time-null_time));
    let instant = Instant::now();
    for n in 1..iterations {
        mask_call(iterations);
    }
    let mask_time = instant.elapsed();
    println!("mask   : {:?} {:?}",mask_time,(mask_time-null_time));
}
