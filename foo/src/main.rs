extern crate foo;

fn main() {
    foo::init();
    println!("{:?}", foo::sqrt(0f64));
    println!("{:?}", foo::sqrt(-0f64));
    println!("{:?}", foo::sqrt(1f64));
    println!("{:?}", foo::sqrt(-1f64));
    println!("{:?}", foo::sqrt(4f64));
    println!("{:?}", foo::sqrt(-4f64));
    println!("{:?}", foo::sqrt(100f64));
    println!("{:?}", foo::sqrt(-100f64));
    println!("Hello, world!");
}
