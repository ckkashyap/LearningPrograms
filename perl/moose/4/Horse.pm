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


my @pp=("name", "abc",'size');

my $count=0;
for my $x (@pp){
	print "-->$x\n";
has $x => (
      is => 'rw',
      default =>
          sub { $x },
  );
	$count++;
}



#use Trait;
#sub apply{
#Moose::Util::apply_all_roles(@_,Trait->meta);
##__PACKAGE__->meta->apply(@_);
#
#
#}



__PACKAGE__->meta()->make_immutable();
no Moose;
1;
