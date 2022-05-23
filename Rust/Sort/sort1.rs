// Function types must be declared
fn print_arr(a: &[i32]) -> () {
  for i in a {
    print!("{} ", i);
  }
  println!("");
}

fn swap(a: &mut[i32], i: usize, j: usize) -> () {
  let tmp = a[i];
  a[i] = a[j];
  a[j] = tmp;
}

fn sort<F>(a: &mut[i32], test: F) -> ()
    where F: Fn(i32,i32) -> bool {
  for i in 0..a.len() {
    for j in i..a.len() {
      //if a[i] > a[j] {
      if test(a[i], a[j]) {
        swap(&mut a[..], i, j);
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

