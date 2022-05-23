use std::fmt::Display;

// Function types must be declared
fn print_arr<T: Display>(a: &[T]) -> () {
  for i in a {
    print!("{} ", i);
  }
  println!("");
}

fn sort<T: Copy, F>(a: &mut[T], test: F) -> ()
    where F: Fn(T,T) -> bool {
  for i in 0..a.len() {
    for j in i..a.len() {
      if test(a[i], a[j]) {
        a.swap(i,j);
      }
    }
  }
}

fn main() {
  let mut nums = [9, 4, 13, 2, 22, 17, 8, 9, 1];
  let asc = |x:i32, y:i32| { x > y };
  print_arr(&nums[..]);
  sort(&mut nums[..], asc);
  print_arr(&nums[..]);
}

