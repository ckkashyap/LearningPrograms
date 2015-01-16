struct S {
	x: u32,
	y: &'static str,
	
}

static mut x : S =  S { x:10 , y:"ABCD" };


fn f1( s :&mut S, xx : &'static str) {
	println!("{}", s.x);
	println!("{}", s.y);
	s.x = 30;
	s.y = xx;
}

fn main(){
	println!("hello");
	unsafe {
	x.x = 20;
	f1(& mut x, "AAA");
	f1(& mut x, "BBB");
	f1(& mut x, "BBB");
	}
}
