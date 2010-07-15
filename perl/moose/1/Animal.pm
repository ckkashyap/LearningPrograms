package Animal;

use Moose;
 
has 'name' => (is => 'rw');
has 'color' => (is => 'rw',default=>'blue');

1;
