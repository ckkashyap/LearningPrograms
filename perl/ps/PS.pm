package PS;

use strict;
use warnings;


sub new{
	my$class=shift;
	my%params=@_;
	$params{output}="out.ps" unless $params{output};
	$params{PS}=[
	"%!",
	"/Times-Roman findfont",
	"10 scalefont",
	"setfont",
	];
	$params{X}=0;
	$params{Y}=0;
	bless \%params,$class;
}


sub locate{
	my($self,$x,$y)=@_;
	$self->{X}=$x;
	$self->{Y}=$y;
}


sub line{
	my($self,$x1,$y1,$x2,$y2,$red,$green,$blue)=@_;
	$red=0 unless $red;
	$green=0 unless $green;
	$blue=0 unless $blue;
	push @{$self->{PS}},"newpath";
	push @{$self->{PS}},"$x1 $y1 moveto";
	push @{$self->{PS}},"$x2 $y2 lineto";
	push @{$self->{PS}},"gsave";
	push @{$self->{PS}},"$red $green $blue setrgbcolor";
	push @{$self->{PS}},"stroke";
	push @{$self->{PS}},"grestore";
}

sub print{
	my($self,$message,$x,$y,$red,$green,$blue,$angle)=@_;
	$red=0 unless $red;
	$green=0 unless $green;
	$blue=0 unless $blue;
	$angle=0 unless $angle;
	push @{$self->{PS}},"gsave";
	push @{$self->{PS}},"newpath";
	push @{$self->{PS}},"$x $y moveto";
	push @{$self->{PS}},"$angle rotate";
	push @{$self->{PS}},"$red $green $blue setrgbcolor";
	push @{$self->{PS}},"($message) show";
	push @{$self->{PS}},"gsave";
	push @{$self->{PS}},"stroke";
	push @{$self->{PS}},"grestore";
	push @{$self->{PS}},"grestore";
}

sub vprint{
	my($self,$message,$x,$y,$red,$green,$blue)=@_;
	$self->print($message,$x,$y,$red,$green,$blue,90);
}



sub save{
	my($self)=@_;
	my$fileName=$self->{output};
	open OUT,">$fileName";
	for(@{$self->{PS}}){
		print OUT;
		print OUT "\n";
	}
}


1;
