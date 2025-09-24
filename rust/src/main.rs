fn fib(n: usize) -> usize {
    match n {
        0 => 0,
        1 => 1,
        n => fib(n - 1) + fib(n - 2),
    }
}

fn fact(n: usize) -> usize {
    match n {
        0 => 1,
        n => n * fact(n - 1),
    }
}

fn fact2(n: usize) -> usize {
    let nums = 1..n;
    nums.fold(1, |x, xs| x * xs)
    // nums.product()
}

fn fib_tail_recursive(nth: usize) -> usize {
    fn fib_tail_iter(n: usize, prev_fib: usize, fib: usize) -> usize {
        match n {
            0 => prev_fib,
            n => fib_tail_iter(n - 1, fib, prev_fib + fib),
        }
    }
    fib_tail_iter(nth, 0, 1)
}

fn main() {
    let n = 10;
    let res = fact(n);
    let res2 = fact2(n);
    println!("{res}");
    println!("{res2}");
}
