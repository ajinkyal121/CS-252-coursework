struct Complex { real: i32, imaginary: i32 }

fn add_complex(c1: &Complex, c2: &Complex) -> Complex {
  let r = c1.real + c2.real;
  let i = c1.imaginary + c2.imaginary;
  Complex { real: r, imaginary: i }
}

fn main() {
  let cmplx1 = Complex { real: 7, imaginary: 2 };
  let cmplx2 = Complex { real: 3, imaginary: 1 };
  let ans = add_complex(&cmplx1, &cmplx2);

  println!("{}+{}i + {}+{}i = {}+{}i", cmplx1.real, cmplx1.imaginary, cmplx2.real, cmplx2.imaginary, ans.real, ans.imaginary);
  
}

