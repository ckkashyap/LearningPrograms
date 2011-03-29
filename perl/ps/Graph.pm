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
	push @{$self->{TYPEWISE}->{$valueType}->{values}},{x=>$x,y=>$y};
	my $tMaxx=$self->{maxx} || $x;
	my $tMaxy=$self->{maxy} || $y;
	my $tMinx=$self->{minx} || $x;
	my $tMiny=$self->{miny} || $y;

	$self->{maxx} = $x if $tMaxx <= $x;
	$self->{maxy} = $y if $tMaxy <= $y;
	$self->{minx} = $x if $tMinx >= $x;
	$self->{miny} = $y if $tMiny >= $y;
}

my @colorTable=(
[.3,.5,.5],
[.3,.3,.5],
[.3,.3,.3],
);

sub save{
	my($self,$name)=@_;
	my $ps = new PS(output=>$name);
	$self->createGrids($ps);
	my$colorIndex=0;
	for my $vt (keys %{$self->{TYPEWISE}}){
		my $red=$colorTable[$colorIndex]->[0];
		my $green=$colorTable[$colorIndex]->[1];
		my $blue=$colorTable[$colorIndex]->[2];
		$colorIndex++;
		my($px,$py);
		my $maxx = $self->{maxx};
		my $maxy = $self->{maxy};
		for my $v (@{$self->{TYPEWISE}->{$vt}->{values}}){
			my$x=(($v->{x})/$maxx)*500;
			my$y=(($v->{y})/$maxy)*500;
			$px=$px || $x;
			$py=$py || $y;
			$ps->circle($x,$y,2,1,0,0);
			$ps->line($px,$py,$x,$y,$red,$green,$blue);
			$px=$x;
			$py=$y;
		}
	}
	$ps->save
}

sub createGrids{
	my ($self,$ps)=@_;
	$ps->line(10,50,560,50,1,.9,.5);
	$ps->line(50,10,50,560,1,.9,.5);
	for(my $y=60;$y<=550;$y+=10){
		$ps->line(51,$y,550,$y,1,0.9,0.9);
	}
	for(my $x=60;$x<=550;$x+=10){
		$ps->line($x,51,$x,550,1,.9,.9);
	}
	return $ps;
}

1;
