use strict;
use warnings;
use VNC::Server;


my $type = 1;



sub updater {
	my ($vnc, $inc, $x,$y,$w,$h) = @_ ;

	my $codec = $vnc->{CODEC};


	my $data ;
	
	if ($type==1) {
		$data= $codec->encode_framebuffer_update($codec->encode_raw_rect($codec->encode_pixel(0,0,255), $x,$y,$w,$h) ) ;
	} else {
		$data = $codec->encode_framebuffer_update(	$codec->encode_rre($codec->encode_pixel(0,255,0), $x,$y,$w,$h,
													$codec->encode_rre_subrect( $codec->encode_pixel(255,255,255), 10,10,20,10),
													$codec->encode_rre_subrect( $codec->encode_pixel(0,128,128), 10,20,10,20)
												)) ;
	}

	return $data ;
}

my $s = new VNC::Server( Port=>2, 
						 Idle=>1, 
						 Caption=>'Hello From perlVNC',
						 Width => 50, Height => 50,
						 Password => 'dummy',
						 IdleHandler => sub {print "Nothing happened for 1 second...\n"},
						 KeyEventHandler => sub { my ($vnc,$d,$k) = @_; print "down = $d, key = $k\n";},
						 PointerEventHandler => \&pointer,
						 UpdateDisplayHandler => \&updater
						);

$s->vnc_server_loop;

sub pointer {
	my ($vnc,$mask,$x,$y) = @_ ;

	if ($mask==1) {
		$type=1 ;
	};
	if ($mask==4) {
		$type=2;
	}
	if ($mask==2) {
		$vnc->disconnect ;
		return ;
	}

	$s->update_all_displays;
}
