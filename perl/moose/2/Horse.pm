package Horse;
use Moose;
with 'Animal';

sub sound{
	'neigh'
}
no Moose;
1;
