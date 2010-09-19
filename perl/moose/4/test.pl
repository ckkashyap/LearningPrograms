use Horse;
use Trait;

$x=Horse->new(name=>"abc",color=>"red");


 $x->speak;


 print "\n123---\n";
 print $x->name;
 print "\n---\n";
 
$x->name("hello world\n");

 print "\n---\n";
 print $x->name;
 print "\n---\n";


$x -> Trait::apply;
 print $x->abcd;
 print "\n";
