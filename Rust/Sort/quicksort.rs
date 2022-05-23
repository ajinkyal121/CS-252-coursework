use std::fmt::Display;

// Function types must be declared
fn print_arr<T: Display>(a: &[T]) -> () {
  for i in a {
    print!("{} ", i);
  }
  println!("");
}

fn quicksort<T,F>(a: &mut [T], test: &F) 
    where F: Fn(&T,&T) -> bool
{
    let size = a.len();
    if size >= 2 {
        let pivot = partition(a, test);
        quicksort(&mut a[0..pivot], test);
        quicksort(&mut a[pivot + 1..size], test);
    }
}
 
fn partition<T,F>(a: &mut [T], test: &F) -> usize 
    where F: Fn(&T,&T) -> bool
{
    let size = a.len();
    let pivot = size / 2;
    let last = size - 1;
    a.swap(pivot, last);
    let mut index = 0;
    for i in 0..last {
        if test(&a[i], &a[last]) {
            a.swap(i, index);
            index += 1;
        }
    }
    a.swap(index, size - 1);
    index
}

fn main() {
    let mut nums = [9, 4, 13, 2, 22, 17, 8, 9, 1];
    print_arr(&nums[..]);
    quicksort(&mut nums[..], &|x,y| x < y);
    print_arr(&nums[..]);
}

