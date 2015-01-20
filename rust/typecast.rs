struct S {
	x: u8,
	y: u8, 
}

struct T {
	a: u32,
}

static mut x : S =  S { x:1 , y: 1 };

fn main(){

	let b : * mut T;
	unsafe {
		let a : * mut S = & mut x;
		let p : * mut u64 = a as * mut u64;
		let b = p as * mut T;
		
		println!("b= {}", (*b).a);
	}
}
