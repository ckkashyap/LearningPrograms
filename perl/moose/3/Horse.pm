package Horse;
use Moose;
with 'Animal';

sub sound{
	'neigh'
}

before 'speak' => sub{
	my($x)=shift;
	my@list=keys %$x;
	my $y=blessed $x;
	print "This is before speak <$y> @list \n";
};

after 'speak' => sub{
	my($x)=shift;
	my@list=keys %$x;
	my $y=blessed $x;
	print "This is after speak <$y> @list \n";
};

around 'speak' => sub{
	my($next,$self)=@_;
	print "ENTER AROUND\n";
	
	$self->$next(@_);

	print "EXIT AROUND\n";
};

no Moose;
1;
