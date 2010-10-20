package VNC::ServerConnection;

require 5.002;   # becase we use prototypes

use Socket;
use warnings;
use strict;
use VNC::Codec;
use vars qw(@ISA @EXPORT_OK);
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw();


sub STATE_UNINITIALIZED				{0};
sub STATE_WAIT_FOR_PROTOCOL			{1};
sub STATE_WAIT_FOR_AUTHENTICATION	{2};
sub STATE_WAIT_FOR_CLIENT_INIT_MSG	{3};
sub STATE_WAIT_FOR_MSG_HEADER		{4};
sub STATE_WAIT_FOR_FIXED_LEN_MSG	{5};
sub STATE_WAIT_FOR_TEXT_CUT_LENGTH	{6};
sub STATE_WAIT_FOR_TEXT_CUT_CONTENT {7};
sub STATE_WAIT_FOR_ENCODING_COUNT	{8};
sub STATE_WAIT_FOR_ENCODING_DATA	{9};

sub MSG_SET_PIXEL_FORMAT 			{0};
sub MSG_FIX_COLOR_MAP_ENTRIES 		{1};
sub MSG_SET_ENCODING 				{2};
sub MSG_FRAME_BUFFER_UPDATE_REQUEST {3};
sub MSG_KEY_EVENT					{4};
sub MSG_POINTER_EVENT				{5};
sub MSG_CLIENT_CUT_TEXT				{6};

my @rfb_message_handlers =( [\&do_set_pixel_format,	20],			#SetPixelFormat 
							[\&do_fix_color_map_entries, 4],		#FixColorMapEntries
							[\&do_set_encoding, -1],				#SetEncoding
							[\&do_frame_buffer_update_request,10], 	#FramebufferUpdateRequest 
							[\&do_key_event, 8],					#KeyEvent
							[\&do_pointer_event,6],					#Pointer Event
							[\&do_client_cut_text,-1]
							);

######################
#
# Helper function to build the ServerInitMessage binary struct (please see the VNC documentation)
#
sub BUILD_SERVER_INIT_MSG {
	my ($width, $height, $pixel_format, $name) = @_ ;

	my $buffer = pack ( "nn", $width, $height ) . $pixel_format . pack("N", length($name)) . $name; 

	return $buffer ;
}


#######
# Constructs a new, VNC server object, connected to one client using socket handle
#
sub new {
	my $proto = shift;
	my $class = $proto;

	my %args = @_ ;

	my $sock_handle = $args{Socket}; 
	die "$proto->new called without any socket handle!" unless (defined $sock_handle );

	my $self  = {};
	bless ($self, $class);

	$self->{HANDLE} = $sock_handle;

	$self->{STATE}		= STATE_UNINITIALIZED ;
	$self->{RECV_BUFFER} = '' ;
	$self->{EXPECTED_BUFFER_SIZE} = 0 ;

	$self->{CODEC} = new VNC::Codec();

	#if this is defined, a response will be sent immediatly
	#when a FrameBufferUpdateRequest is received
	#if this is UNDEFINED, no response will be sent, and UPDATE_REQUEST_PENDING will be set
	$self->{SEND_UPDATE_RESPONSE} = 1 ;

	#if this is defined, it means an update request was received, 
	#but a response was not sent.
	#so the next time you call udpate_frame_buffer a response will be sent immediately
	$self->{UPDATE_REQUEST_PENDING} = 1 ;

	$self->{Server} = $args{Server} ;

	#handlers
	$self->{KeyEventHandler} = $args{KeyEventHandler};
	$self->{PointerEventHandler} = $args{PointerEventHandler};
	$self->{CutTextHandler} = $args{CutTextHandler};
	$self->{UpdateDisplayHandler} = $args{UpdateDisplayHandler};

	#Get the pixel format
	#if none specified, default to 32Bits/Pixel 24bit depth, RGB888
	my $pixel_format =  $args{PixelFormat} || VNC::Codec::VNC_PIXELFORMAT_32BPP_RGB888;
	$self->{PixelFormat} = $pixel_format;
	
    my (undef, $myaddr) = sockaddr_in(getsockname($sock_handle));
	$self->{Caption} = $args{Caption} || "PerlVNC Server at " . scalar gethostbyaddr($myaddr, AF_INET) . " [" . inet_ntoa($myaddr). "]" ;


	$self->{Width} = $args{Width} || 200;
	$self->{Height} = $args{Height} || 200;

	$self->{PENDING_UPDATE_REQUEST_BUFFER} = pack("CCnnnn",0,0,0,0,10,10);

	$self->{Password} = $args{Password};

	return $self;
}

##########################################3
#
# Helper function to send data to the socket
#
sub send_buffer($$) {
	my $self = shift ;
	my $a = shift ;
	die "send_buffer called, when self is undef"  unless (defined $self);
	return unless (defined $self->{HANDLE});

	if (!defined $a) {
		warn "send_buffer called, buffer is empty";
		return ;
	}

	my $count =	send($self->{HANDLE}, $a,0);

	die "Error sending buffer ($a) " unless ($count == length $a);

	return $count;
}


###############################
# Given a SET_PIXEL_FORMAT buffer (20 bytes long, see VNC documentation)
# Sets the pixel format of this session
sub do_set_pixel_format {
	my $self = shift ;
	my $buffer = shift ;

	die "do_set_pixel_format called, when self is undef"  unless (defined $self);
	die "do_set_pixel_format, buffer isn't 20 bytes long" unless (length $buffer==20);

	my (undef,undef,undef,undef,$bits_per_pixel, 
		$depth ,
		$big_endian,
		$true_color,
		$red_max,$green_max,$blue_max,
		$red_shift,$green_shift, $blue_shift) = 
		unpack("CCCCCCCCnnnCCC", $buffer) ;

	#
	# TODO: Let's do some sanity checks
	#

	$self->{CODEC}->set_pixel_format($bits_per_pixel, 
							$depth, $big_endian, $true_color,
							$red_max,$green_max,$blue_max,
							$red_shift,$green_shift, $blue_shift);
	
	return ;
}

sub do_fix_color_map_entries() {
	my $self = shift ;
	die "do_fix_color_map_entries called, when self is undef"  unless (defined $self);

	my $buffer = shift;
	die "do_set_pixel_format, buffer isn't 6 bytes long" unless (length $buffer==6);

	die "Client Sent rfbFixColourMapEntriesMsg - UNSUPPORTED\n";

	my (undef,$pad,$first_color,$n_colors) = unpack("CCnn", $buffer) ;

	print "color map, first color = $first_color, total colors = $n_colors\n" ;
	return ;
}

sub do_set_encoding() {
	my $self = shift ;
	die "do_set_encoding called, when self is undef"  unless (defined $self);
	
	my $buffer = shift;

	my (undef,undef,$nEncoding,@encodings) = unpack("CCnN*", $buffer) ;

	#
	# Debug Only
	#
	print "Client supports $nEncoding encodings: ",join(' ', @encodings),"\n";

	return ;
}

sub update_display {
	my $self = shift ;
	die "update_display called, when self is undef"  unless (defined $self);

	#if we received an update request but did not send  response, we'll send a response immediately
	if (defined $self->{UPDATE_REQUEST_PENDING}) {

		my (undef,$incremental, $x,$y,$w,$h) = unpack("CCnnnn", $self->{PENDING_UPDATE_REQUEST_BUFFER}) ;
		print "===> $incremental $x $y $w $h\n";

		my $handler = $self->{UpdateDisplayHandler};
		if (defined $handler) {
			my $data = &$handler($self, $incremental, $x, $y, $w, $h);

			$self->send_buffer($data);
		}

		$self->{UPDATE_REQUEST_PENDING} = undef;
		$self->{SEND_UPDATE_RESPONSE} = undef;
	} else {

		#if now update request is pending, 
		#we'll simply signal the next request to be answered immediately
		$self->{SEND_UPDATE_RESPONSE} = 1 ;
	}
}
sub disconnect {
	my $self = shift;
	
	my $server = $self->{Server} ;

	die "server=undef" unless (defined $server);

	return unless $self->{HANDLE};

	$server->disconnect_client($self,$self->{HANDLE}) ;

	$self->{HANDLE} = undef ;
	$self = undef ;
}

sub do_frame_buffer_update_request() {
	my $self = shift ;
	die "do_frame_buffer_update_request called, when self is undef"  unless (defined $self);

	my $buffer = shift;
	die "do_frame_buffer_update_request, buffer isn't 10 bytes long" unless (length $buffer==10);

	my (undef,$incremental, $x,$y,$w,$h) = unpack("CCnnnn", $buffer) ;


#	print "UpdateRequest: Incremental=$incremental, X,Y=($x,$y)  WxH = ($w x $h)\n";

	if (defined $self->{SEND_UPDATE_RESPONSE}) {
		my $codec = $self->{CODEC};

	#	my $data = $codec->encode_framebuffer_update($codec->encode_raw_rect($codec->encode_pixel(0,0,255), $x,$y,$w,$h) ) ;

#		my $data = $codec->encode_framebuffer_update(	$codec->encode_rre($codec->encode_pixel(0,255,0), $x,$y,$w,$h,
#														$codec->encode_rre_subrect( $codec->encode_pixel(255,255,255), 10,10,20,10),
#														$codec->encode_rre_subrect( $codec->encode_pixel(0,128,128), 10,20,10,20)
#													)
#												) ;	

		my $handler = $self->{UpdateDisplayHandler};
		if (defined $handler) {
			my $data = &$handler($self, $incremental, $x, $y, $w, $h);

			$self->send_buffer($data);
		}

		$self->{UPDATE_REQUEST_PENDING} = undef;
		$self->{SEND_UPDATE_RESPONSE} = undef;

	} else {

		$self->{PENDING_UPDATE_REQUEST_BUFFER} = $buffer ;

		$self->{UPDATE_REQUEST_PENDING} = 1;
	}

	return ;
}

sub do_key_event() {
	my $self = shift ;
	die "do_key_event called, when self is undef"  unless (defined $self);

	my $buffer = shift;
	die "do_key_event, buffer isn't 8 bytes long" unless (length $buffer==8);

	my (undef,$down,$pad,$key) = unpack("CCnN", $buffer) ;

	my $handler = $self->{KeyEventHandler};
	if (defined $handler) {
		&$handler($self, $down, $key);
	}
	return ;
}

sub do_pointer_event() {
	my $self = shift ;
	die "do_pointer_event called, when self is undef"  unless (defined $self);

	my $buffer = shift;
	die "do_pointer_event, buffer isn't 6 bytes long" unless (length $buffer==6);

	my (undef,$mask,$x,$y) = unpack("CCnn", $buffer) ;

	my $handler = $self->{PointerEventHandler};
	if (defined $handler) {
		&$handler($self, $mask, $x,$y);
	}
	return ;
}

sub do_client_cut_text() {
	my $self = shift ;
	die "do_client_cut_text called, when self is undef"  unless (defined $self);

	my $buffer = shift;
	print "Client Sent client cut text\n";

	my (undef,undef,$length,$text) = unpack("CnNA*", $buffer) ;

	my $handler = $self->{CutTextHandler};
	if (defined $handler) {
		&$handler($self, $text);
	}
	return ;
}

sub start_conversation() {
	my $self = shift ;
	die "handle_client_data called, when self is undef"  unless (defined $self);

	my $fh = $self->{HANDLE};
	die "start_convertsion, handle is undef" unless (defined $fh);

	$self->send_buffer("RFB 003.003\n");
	$self->{STATE} = STATE_WAIT_FOR_PROTOCOL ;
	$self->{EXPECTED_BUFFER_SIZE} = 12 ;
}

sub handle_client_data() {
	my $self = shift ;
	die "handle_client_data called, when self is undef"  unless (defined $self);

	my $fh = $self->{HANDLE};
	die "start_convertsion, handle is undef" unless (defined $fh);

	#Read data from the socket
	my $data ;
	my $read_rc = $fh->sysread($data, 32786) ;
	
	if (defined $read_rc) {
		if ($read_rc > 0) {

			#data read OK
			#add it to our buffer, check if we got enough data for the current state
			$self->{RECV_BUFFER} .= $data ;

			while ( length $self->{RECV_BUFFER} >= $self->{EXPECTED_BUFFER_SIZE} ) {
				#got enough data from current state, so handle the state
					
				#debug
#				print "Handle Buffer: expected size = ",$self->{EXPECTED_BUFFER_SIZE}," recv_buffer size = ", length($self->{RECV_BUFFER}),"\n";
					
				#take just the needed number of byte
				my $buffer = substr $self->{RECV_BUFFER},0,$self->{EXPECTED_BUFFER_SIZE};

				#leave the remaining bytes in the buffe
				$self->{RECV_BUFFER} = substr ($self->{RECV_BUFFER}, $self->{EXPECTED_BUFFER_SIZE}, length($self->{RECV_BUFFER})-$self->{EXPECTED_BUFFER_SIZE} ); 
				
				#debug
#				print "After Buffer Split: buffer size = ", length($buffer), " recv_buffer size = ",length($self->{RECV_BUFFER}),"\n" ;
				
				$self->handle_state($buffer);
			}
		} else {
			#read_rc == 0
			#it means the socket was closed by the client
				
				
			return undef;
		}
	} else { 
#		die "Errro while reading socket";
		
		return undef;	
	}
	
	return 1;
}

sub handle_state {
	my $self = shift ;
	die "handle_state called, when self is undef"  unless (defined $self);
	
	my $buffer = shift ;
	die "handle_state called, when buffer is undef"  unless (defined $buffer);
	
	my $current_state = $self->{STATE};

	#
	# Got protocol version string from the cilent
	#	
	if ($current_state==STATE_WAIT_FOR_PROTOCOL) {
		if (length($buffer) != 12) {
			warn "Error: WAIT_FOR_PROTOCOL state, buffer != 12 bytes" ;
			return undef;
		};
		
		my ($major, $minor) = $buffer =~ m/^RFB (\d{3})\.(\d{3})$/ ;
		die "unknown protocol version ($buffer)" unless (defined $major && defined $minor);
		if ($major != 3) {
			$self->send_buffer("Uknown version. i need major protocol version = 3\nBye Bye\n");
			return undef;
		}
		print "Client requested protocol version $major.$minor\n";
		
		#
		# Next step:
		# Ask for password, if needed
		#

#		if (defined $self->{Password}) {
#			print "SENDING PASSWORD\n";
#			$self->{STATE} = STATE_WAIT_FOR_AUTHENTICATION ;
#			$self->{EXPECTED_BUFFER_SIZE} = 16 ;
#			
#			$self->send_buffer(pack("N",2));
#
#			# TODO:
#			# send a challange and check the resonse using DES
#			$self->send_buffer("0123456789abcdef");
#		}
#		else {
			# no password needed
			$self->{STATE} = STATE_WAIT_FOR_CLIENT_INIT_MSG ;
			$self->{EXPECTED_BUFFER_SIZE} = 1 ;
			
			print "SEDING BACK 1\n";
			$self->send_buffer(pack("N",1));
			#}
	} 
	elsif
	#
	# Got Authentication response from the client
	#
	($current_state==STATE_WAIT_FOR_AUTHENTICATION) {
		if (length($buffer) != 16) {
			warn "Error: WAIT_FOR_AUTHENTICATION state, buffer != 16 bytes" ;
			return undef;
		};
		
		#for now, accept any password
		$self->send_buffer(pack("N",0));

		#
		# Next Stap:
		# Client Init Msg
		$self->{STATE} = STATE_WAIT_FOR_CLIENT_INIT_MSG;
		$self->{EXPECTED_BUFFER_SIZE} = 1 ;
	}
	elsif
	#
	# Got client Init Message from the client
	#
	($current_state==STATE_WAIT_FOR_CLIENT_INIT_MSG) {
		if (length($buffer) != 1) {
			warn "Error: WAIT_FOR_CLIENT_INIT_MSG state, buffer != 1 bytes" ;
			return undef;
		};
		my $srvr_init_msg = BUILD_SERVER_INIT_MSG( $self->{Width}, 
												   $self->{Height},
												   $self->{PixelFormat},
												   $self->{Caption} );
		$self->send_buffer($srvr_init_msg);
		
		#
		# Next Step
		# client normal messages
		$self->{STATE} = STATE_WAIT_FOR_MSG_HEADER;
		$self->{EXPECTED_BUFFER_SIZE} = 1 ;
	}
	elsif
	#
	# Got Message header from the client
	#
	($current_state==STATE_WAIT_FOR_MSG_HEADER) {
		print "GOT MESSAGE FROM CLIENT\n";
		print length $buffer;
		print"\n";
		print unpack "H*",$buffer;
		print "\n";
		if (length($buffer) != 1) {
			warn "Error: STATE_WAIT_FOR_MSG_HEADER state, buffer != 1 bytes" ;
			return undef;
		};
		
		my $msg = ord($buffer) ;
		print "MESSAGE = $msg\n";
		
		my ($subroutine, $byte_count) = @{$rfb_message_handlers[$msg]};

		print "sub=$subroutine $byte_count\n";
		
		die "Error: unknown message $msg" unless (defined $subroutine && defined $byte_count);		

		# These are FixedLength messages, we can handle them all in the same way
		if ( $msg==MSG_SET_PIXEL_FORMAT
			 or 
			 $msg==MSG_FRAME_BUFFER_UPDATE_REQUEST
			 or
			 $msg==MSG_KEY_EVENT
			 or 
			 $msg==MSG_POINTER_EVENT ) 
		{
			print "MSG_SET_PIXEL_FORMAT\n";
			#
			# Next Step
			# client normal messages
			$self->{STATE} = STATE_WAIT_FOR_FIXED_LEN_MSG;
			$self->{EXPECTED_BUFFER_SIZE} = $byte_count ;
			
			#re-insert this byte into the buffer
			$self->{RECV_BUFFER} = pack("C",$msg) . $self->{RECV_BUFFER} ;
			$self->{CURRENT_MSG} = $msg ;
			
		} elsif 
		($msg==MSG_SET_ENCODING) {
			$self->{STATE} = STATE_WAIT_FOR_ENCODING_COUNT;
			$self->{EXPECTED_BUFFER_SIZE} = 3 ;
			
			$self->{ENCODING_BUFFER} = $buffer;
		} elsif
		($msg==MSG_CLIENT_CUT_TEXT) {
			$self->{STATE} = STATE_WAIT_FOR_TEXT_CUT_LENGTH;
			$self->{EXPECTED_BUFFER_SIZE} = 7 ;
			$self->{CUT_TEXT_BUFFER} = $buffer;
		}
	}
	elsif
	#
	# Got Message header from the client
	#
	($current_state==STATE_WAIT_FOR_FIXED_LEN_MSG) {
			print "HELLOWORLD\n";
		my $msg = $self->{CURRENT_MSG};
		my ($subroutine, $byte_count) = @{$rfb_message_handlers[ $msg ]};
		die "Error: unknown message $msg" unless (defined $subroutine && defined $byte_count);		

		if (length($buffer) != $byte_count) {
			warn "Error: STATE_WAIT_FOR_FIXED_LEN_MSG state (msg=$msg), buffer != $byte_count bytes" ;
			return undef;
		};

		#
		# Next Step
		# Execute the client's request
		&$subroutine($self,$buffer);

		$self->{STATE} = STATE_WAIT_FOR_MSG_HEADER;
		$self->{EXPECTED_BUFFER_SIZE} = 1 ;
	} elsif
	#
	# Got Encoding Count
	#
	($current_state==STATE_WAIT_FOR_ENCODING_COUNT) {
		if (length($buffer) != 3) {
			warn "Error: STATE_WAIT_FOR_ENCODING_COUNT state, buffer != 3 bytes" ;
			return undef;
		};
	
		my (undef,$nEncoding) = unpack("Cn", $buffer) ;
		print "Client supports $nEncoding encodings\n" ;
		
		$self->{STATE} = STATE_WAIT_FOR_ENCODING_DATA;
		$self->{EXPECTED_BUFFER_SIZE} = $nEncoding*4 ;
		$self->{ENCODING_BUFFER} .= $buffer;
	} elsif
	#
	# Got Encoding Data
	#
	($current_state==STATE_WAIT_FOR_ENCODING_DATA) {
		$self->{ENCODING_BUFFER} .= $buffer;
		
		$self->do_set_encoding($self->{ENCODING_BUFFER});

		$self->{STATE} = STATE_WAIT_FOR_MSG_HEADER;
		$self->{EXPECTED_BUFFER_SIZE} = 1 ;
	} elsif
	#
	# Got Client Cut Text Length
	#
	($current_state==STATE_WAIT_FOR_TEXT_CUT_LENGTH) {
		if (length($buffer) != 7) {
			warn "Error: STATE_WAIT_FOR_TEXT_CUT_LENGTH state, buffer != 7 bytes" ;
			return undef;
		};
	
		my (undef,undef,$length) = unpack("CnN", $buffer) ;
		print "Client Cut Text: $length characters \n" ;
		
		$self->{STATE} = STATE_WAIT_FOR_TEXT_CUT_CONTENT;
		$self->{EXPECTED_BUFFER_SIZE} = $length ;
		$self->{TEXT_CUT_BUFFER} .= $buffer;
	} elsif
	#
	# Got Client Cut text content
	#
	($current_state==STATE_WAIT_FOR_TEXT_CUT_CONTENT) {
		$self->{TEXT_CUT_BUFFER} .= $buffer;
		
		$self->do_client_cut_text($self->{TEXT_CUT_BUFFER});
		$self->{TEXT_CUT_BUFFER} = undef;

		$self->{STATE} = STATE_WAIT_FOR_MSG_HEADER;
		$self->{EXPECTED_BUFFER_SIZE} = 1 ;
	} ;
}

1;
