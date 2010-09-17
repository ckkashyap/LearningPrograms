package Animal;

use Moose::Role;
 
has 'name' => (is => 'rw');
has 'color' => (is => 'rw',default=>'blue');

sub speak{
	my $self = shift;
	print $self->name, " goes ", $self->sound, "\n";
}

requires 'sound';
no Moose::Role;
1;
