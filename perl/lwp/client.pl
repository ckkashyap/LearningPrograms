use LWP::UserAgent;

$ua = LWP::UserAgent->new;
$ua->agent("MyApp/0.1 ");

# Create a request
my $req = HTTP::Request->new(POST => 'http://search.cpan.org/');
$req->content_type('application/x−www−form−urlencoded');
$req->content('query=libwww−perl&mode=dist');

# Pass request to the user agent and get a response back
my $res = $ua->request($req);

# Check the outcome of the response
if ($res->is_success) {
	print $res->content;
}
else {
	print $res->status_line, "\n";
}
