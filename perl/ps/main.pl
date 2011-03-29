use PS;


my $x=new PS;

$x->line(10,10,100,100,1,0,0);
$x->print("Hello\nWorld", 50,50,0,1,0);
$x->vprint("Hello\nWorld", 100,100,0,0,1);
$x->line(100,100,100,200,0,0,1);
$x->save;

