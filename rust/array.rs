const N : usize = 3;
struct S {
a: u64,
b: [u64;N],
}

fn main() {
	let y = Box::new (3);
	let x : S = S { a: 10, b:[1,2,3]};
	let mut a : u16 = 0xff00;
	//let ra : &mut u16 = & mut a;
	let p : * mut u16 = & mut a as * mut u16;
	let p1 : * mut u8 = p as * mut u8;
	unsafe {
	println!("hello world {} ", *p1.offset(1) );
	}
}
