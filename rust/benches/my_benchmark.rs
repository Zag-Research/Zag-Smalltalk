#![feature(test)]
#![allow(soft_unstable)]
#![allow(warnings)]
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

/*
 * once_cell               time:   [569.59 ps 580.20 ps 595.87 ps]
 * static_init             time:   [2.5625 ns 2.5805 ns 2.6013 ns]
 * lazy_static             time:   [785.32 ps 807.08 ps 837.60 ps]
 * lazy_init               time:   [2.9278 ns 2.9578 ns 2.9887 ns]
 */
extern crate once_cell;
static v_once_cell: once_cell::sync::Lazy<bool> = once_cell::sync::Lazy::new(|| {
    true
});

fn access_once_cell() -> bool {
    *v_once_cell
}
fn bench_once_cell(c: &mut Criterion) {
    c.bench_function("once_cell", |b| b.iter(|| access_once_cell()));
}


extern crate lazy_init;
extern crate static_init;
#[static_init::dynamic]
static v_static_init: bool = true;

fn access_static_init() -> bool {
    *v_static_init
}
fn bench_static_init(c: &mut Criterion) {
    c.bench_function("static_init", |b| b.iter(|| access_static_init()));
}


#[static_init::dynamic]
static v_lazy_init: lazy_init::Lazy<bool> = lazy_init::Lazy::default();

fn access_lazy_init() -> bool {
    *v_lazy_init.get().unwrap()
}
fn bench_lazy_init(c: &mut Criterion) {
    *v_lazy_init.get_or_create(|| true);
    c.bench_function("lazy_init", |b| b.iter(|| access_lazy_init()));
}


#[macro_use]
extern crate lazy_static;
lazy_static!{
    static ref v_lazy_static: bool = true;
}
fn access_lazy_static() -> bool {
    *v_lazy_static
}
fn bench_lazy_static(c: &mut Criterion) {
    c.bench_function("lazy_static", |b| b.iter(|| access_lazy_static()));
}

criterion_group!(accesses, bench_once_cell,bench_static_init,bench_lazy_static,bench_lazy_init);
criterion_main!(/*benches,*/accesses);

