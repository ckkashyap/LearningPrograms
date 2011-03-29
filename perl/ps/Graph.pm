package Graph;
use strict;
use warnings;

use PS;

sub new{
	my$class=shift;
	my%params=@_;
	bless \%params,$class;
}

sub insertValue{
	my($self,$valueType,$x,$y)=@_;
	push @{$self->{$valueType}->{values}},{x=>$x,y=>$y};
	my $tMaxx=$self->{$valueType}->{maxx} || $x;
	my $tMaxy=$self->{$valueType}->{maxy} || $y;
	my $tMinx=$self->{$valueType}->{minx} || $x;
	my $tMiny=$self->{$valueType}->{miny} || $y;

	$self->{$valueType}->{maxx} = $x if $tMaxx <= $x;
	$self->{$valueType}->{maxy} = $y if $tMaxy <= $y;
	$self->{$valueType}->{minx} = $x if $tMinx >= $x;
	$self->{$valueType}->{miny} = $y if $tMiny >= $y;
}

my @colorTable=(
[.3,.5,.5],
[.3,.3,.5],
[.3,.3,.3],
);

sub save{
	my($self,$name)=@_;
	my $ps = new PS(output=>$name);
	createGrids($ps);
	my$colorIndex=0;
	for my $vt (keys %$self){
		my $red=$colorTable[$colorIndex]->[0];
		my $green=$colorTable[$colorIndex]->[1];
		my $blue=$colorTable[$colorIndex]->[2];
		$colorIndex++;
		print "$vt\n";
		my $maxx = $self->{$vt}->{maxx};
		my $maxy = $self->{$vt}->{maxy};
		my $minx = $self->{$vt}->{minx};
		my $miny = $self->{$vt}->{miny};
		print "$vt maxx = $maxx minx=$minx\n";
		print "$vt maxy = $maxy miny=$miny\n";
		print "-"x20,"\n";
		my($px,$py);
		for my $v (@{$self->{$vt}->{values}}){
			my$x=(($v->{x})/$maxx)*500;
			my$y=(($v->{y})/$maxy)*500;
			$px=$px || $x;
			$py=$py || $y;
			$ps->line($px,$py,$x,$y,$red,$green,$blue);
			$px=$x;
			$py=$y;
		}
	}
	$ps->save
}

sub createGrids{
	my ($ps)=@_;
	$ps->line(10,50,550,50,.5,.5,.5);
	$ps->line(50,10,50,550,.5,.5,.5);
	return $ps;
}

1;
