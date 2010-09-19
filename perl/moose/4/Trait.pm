package Trait;
use Moose::Role;
use Moose::Util;

has 'dingo' =>(
		is => 'rw',
		default => sub { 'She sells sea shells on the sea shore' },
	      );

sub apply{
#Moose::Util::apply_all_roles(@_,Trait->meta);
	has 'abcd' => (
			is => 'rw',
			default => sub { '1234' },
		      );
__PACKAGE__->meta->apply(@_);


}


no Moose::Role;
1;
