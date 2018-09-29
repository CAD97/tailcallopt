use tco::tco;

tco! {
    pub(crate) fn factorial(n: u128) -> u128 {
        fac(n, 1)
    }

    fn fac(n: u128, acc: u128) -> u128 {
        if n > 1 {
            fac(n - 1, acc * n)
        } else {
            acc
        }
    }
}

fn main() {
    for i in 0..35 {
        println!("{}! = {}", i, factorial(i));
    }
}
