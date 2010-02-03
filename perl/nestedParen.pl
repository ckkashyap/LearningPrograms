$N=$ARGV[0];

sub paren{
	my($s,$no,$nc)=@_;
	if($no==$N && $nc==$N){
		print $s,"\n";
	}
	if($no<$N){
		&paren("$s(",$no+1,$nc);
	}
	if($no>$nc){
		&paren("$s)",$no,$nc+1);
	}
}

&paren("",0,0);
