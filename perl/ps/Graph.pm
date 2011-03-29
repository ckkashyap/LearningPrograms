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
	$ps->line(10,50,550,50,.5,.5,.5);
	$ps->line(50,10,50,550,.5,.5,.5);
	return $ps;
}

1;
