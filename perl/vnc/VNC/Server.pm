package VNC::Server;

require 5.002;   # becase we use prototypes

use IO::Select;
use IO::Socket;
use IO::Socket::INET;
use Socket;
use warnings;
use strict;
use VNC::ServerConnection;
use vars qw(@ISA @EXPORT_OK);
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw();

sub new {
	my $proto = shift;
	my $class = $proto;

	my %args = @_ ;

	my $self  = {};
	bless ($self, $class);

	# Get the port number
	my $port = $args{Port} ||= 0 ;
	# Port number less than 100 is treated as display number
	if ($port < 100 ) {$port += 5900;}
	$self->{PORT} = $port ;

	# Get the idle Interval
	$self->{IDLE_INTERVAL} = $args{Idle} || 1 ;
	$self->{IdleHandler} = $args{IdleHandler};
	$self->{KeyEventHandler} = $args{KeyEventHandler};
	$self->{PointerEventHandler} = $args{PointerEventHandler};
	$self->{CutTextHandler} = $args{CutTextHandler};
	$self->{UpdateDisplayHandler} = $args{UpdateDisplayHandler};
	$self->{PixelFormat} = $args{PixelFormat};
	$self->{Caption} = $args{Caption};
	$self->{Width} = $args{Width};
	$self->{Height} = $args{Height};
	$self->{ClientConnectHandler} = $args{ClientConnectHandler};
	$self->{ClientDisconnectHandler} = $args{ClientDisconnectHandler};
	$self->{Password} = $args{Password};

	return $self;
}

sub update_all_displays {
	my ($self) = @_ ;

	my $vncref = $self->{VNC_CONNECTIONS} ;
	my %vncs = %{$vncref} ;

	foreach (values %vncs) {
		$_->update_display ;
	}
}

sub disconnect_client {
	my ($self, $vnc, $sock_handle) = @_ ;

	my $vncref = $self->{VNC_CONNECTIONS} ;
	my %vncs = %{$vncref} ;

	my $sel = $self->{SELECTOR};

	shutdown($sock_handle,2);

	delete $vncs{$sock_handle};
	$sel->remove($sock_handle) ;
	$vnc = undef ;
	$sock_handle = undef ;

	print "Client Disconnectd\n";
}

sub vnc_server_loop {
	my ($self) = @_ ;

	my $listen_sock = new IO::Socket::INET(Listen => 1, LocalPort => $self->{PORT} );
	my $sel	   = new IO::Select( $listen_sock );

	my %vncs = () ;

	$self->{VNC_CONNECTIONS} = \%vncs;
	$self->{SELECTOR} = $sel ;

	while (1) {
		my @ready_handles; 
		if (@ready_handles = $sel->can_read( $self->{IDLE_INTERVAL} ) ) {
			foreach my $fh (@ready_handles) {
				if ($fh == $listen_sock) {
					#accept new connection from client

					my $client_sock = $listen_sock->accept;
					$client_sock->blocking(1);
				
					my $vnc = new VNC::ServerConnection( Server => $self,
														 Caption => $self->{Caption},
														 Socket => $client_sock,
														 Width => $self->{Width},
														 Height => $self->{Height},
														 Password => $self->{Password},
														 PixelFormat => $self->{PixelFormat},
														 KeyEventHandler => $self->{KeyEventHandler},
														 PointerEventHandler => $self->{PointerEventHandler},
														 CutTextHandler => $self->{CutTextHandler},
														 UpdateDisplayHandler => $self->{UpdateDisplayHandler}
										
													   );
					print "Accepted new connection from ",$client_sock->sockhost(),"\n" ;

					my $ok = 1 ;
					my $handler = $self->{ClientConnectHandler};
					if (defined $handler) {
						$ok = &$handler($self, $client_sock, $vnc);
					}

					if (defined $ok) {
						$sel->add($client_sock);
						$vnc->start_conversation();
						$vncs{$client_sock} = $vnc ;
					} else {
						$vnc = undef ;
						shutdown($client_sock,2) ;
						$client_sock = undef; 
					}
				} else {
					my $vnc = $vncs{$fh} ;

					die "Error! got data from un-connected socket" unless (defined $vnc);

					if (!defined $vnc->handle_client_data()) {
							$vnc->disconnect;
					}
				}
			}
		} else {
			my $sub = $self->{IdleHandler};
			if (defined $sub) {
				&$sub($self) ;
			}
		}
	}
}

1;