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

fn main() {
  let mut nums = [9, 4, 13, 2, 22, 17, 8, 9, 1];
  print_arr(&nums[..]);
  for i in 0..nums.len() {
    for j in i..nums.len() {
      if nums[i] > nums[j] {
        swap(&mut nums[..], i, j);
      }
    }
  }
  print_arr(&nums[..]);
}

