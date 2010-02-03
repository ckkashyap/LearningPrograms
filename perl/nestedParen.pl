$N=$ARGV[0];

sub paren{
	($o,$c,$s)=@_;
	if($o==$N && $c==$N){
		print $s,"\n";
		return;
	}
	if($o<$N){
		&paren($o+1,$c,"$s(");
	}
	if($c<$o){
		&paren($o,$c+1,"$s)");
	}
}


&paren(0,0,"");
