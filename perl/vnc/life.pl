use strict;
use warnings;
use VNC::Server;


my $life_cell_pixel_size = 5 ;
my $life_cells_per_col   = 20 ;
my $life_cells_per_row   = 20;

my $running = 0;

#
# Init the life data
#
my @life_data;

foreach my $i (0..$life_cells_per_row-1) {
	foreach my $j (0..$life_cells_per_col-1) {
		$life_data[$i][$j] = int(rand(2));
	}
}

sub count_neighbors {
	my ($i,$j) = @_ ;

	my $count =0 ;
	
	foreach my $q ( ($i-1)..($i+1)) {
		foreach my $w ( ($j-1)..($j+1)) {
			if ( ($q>=0) && ($q<$life_cells_per_row) && 
				 ($w>=0) && ($w<$life_cells_per_col) &&
				 (($q!=$i) || ($w!=$j))
				 ) {
				if ($life_data[$q][$w]==1) {
					$count++ ;
				}
			}
		}
	}
	return $count;
}


my $s = new VNC::Server( Port=>2, 
						 Idle=>1, 
						 Caption=>'Game of VNCLife',
						 Width => $life_cell_pixel_size * $life_cells_per_row, 
						 Height => $life_cell_pixel_size * $life_cells_per_col,
						 IdleHandler => \&next_turn,
						 KeyEventHandler => \&keyboard, 
						 PointerEventHandler => \&pointer,
						 UpdateDisplayHandler => \&updater
						);

$s->vnc_server_loop;

sub keyboard {
	my ($vnc, $down, $key) = @_ ;

	#act only on key released
	if ($down==0) {

		# R = randomize
		if ($key==ord('R') || $key==ord('r')) {
			foreach my $i (0..$life_cells_per_row-1) {
				foreach my $j (0..$life_cells_per_col-1) {
					$life_data[$i][$j] = int(rand(2));
				}
			}
			$s->update_all_displays;
		}

		# C = clear
		if ($key==ord('C') || $key==ord('c')) {
			foreach my $i (0..$life_cells_per_row-1) {
				foreach my $j (0..$life_cells_per_col-1) {
					$life_data[$i][$j] = 0;
				}
			}
			$s->update_all_displays;
		}

		# Q = Quit
		if ($key==ord('Q') || $key==ord('q')) {
			$vnc->disconnect();
		}

		# S = Start/Stop
		if ($key==ord('S') || $key==ord('s')) {
			if ($running) {
				print "Stoping!\n";
				$running = undef ;
			} else {
				print "Starting!\n";
				$running = 1;
			}
		}
	}
}

sub pointer {
	my ($vnc,$mask,$x,$y) = @_ ;

	if ($mask!=0) {
		my $i = $x / $life_cell_pixel_size ;
		my $j = $y / $life_cell_pixel_size ;

		$life_data[$i][$j] = ($mask==1) ? 1 : 0 ;

		$s->update_all_displays;
	}
}


sub updater {
	my ($vnc, $inc, $x,$y,$w,$h) = @_ ;
	my $codec = $vnc->{CODEC};

	my @rects ;
	my $red_pixel = $codec->encode_pixel(255,0,0);
	my $white_pixel = $codec->encode_pixel(255,255,255);

	foreach my $i (0..$life_cells_per_row-1) {
		foreach my $j (0..$life_cells_per_col-1) {
			if ($life_data[$i][$j]==1) {
				push @rects, $codec->encode_rre_subrect( $red_pixel ,$i*$life_cell_pixel_size, $j*$life_cell_pixel_size,$life_cell_pixel_size,$life_cell_pixel_size);
			}
		}
	}
	my $data = $codec->encode_framebuffer_update( $codec->encode_rre($white_pixel, $x,$y,$w,$h, @rects));

	return $data ;
}

sub next_turn {
	#not running, exit immediately
	return unless ($running);

	#calculate the next life phase
	my @new_life_data;

	foreach my $i (0..$life_cells_per_row-1) {
		foreach my $j (0..$life_cells_per_col-1) {
			$new_life_data[$i][$j] = 0;
		}
	}

	foreach my $i (0..$life_cells_per_row-1) {
		foreach my $j (0..$life_cells_per_col-1) {
			my $current = $life_data[$i][$j];
			my $count = count_neighbors($i,$j);
			
			if ($current==1){
				if ($count<2 || $count>3) {
					$new_life_data[$i][$j] = 0 ;
				} else {
					$new_life_data[$i][$j] = 1 ;
				}
			}else{
				if ($count==3) {
					$new_life_data[$i][$j] = 1 ;
				} else {
					$new_life_data[$i][$j] = 0 ;
				}
			}
		}
	}
	@life_data = @new_life_data;
	
	$s->update_all_displays;
}
