
fn use_pointer(x : &u32) {
	println!("x = {}, Address of x = {:p}", x, &x);
}


fn main() {
	let x:u32 = 1;
	println!("x = {}, Address of x = {:p}", x, &x);
	use_pointer(&x);


	let a:u32 = 1;
	let b = &a;

	println!("{}", *b);
	println!("{:p}", b);
	println!("{}", b);
}
