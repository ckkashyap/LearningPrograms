use Graph;


my $x=new Graph;

$x->insertValue("qps",50,100);
$x->insertValue("qps",60,250);
$x->insertValue("qps",70,300);
$x->insertValue("qps",80,200);

$x->save("hello.ps");
