package VNC::Codec;

require 5.002;   # becase we use prototypes

use Socket;
use warnings;
use strict;
use vars qw(@ISA @EXPORT_OK);
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(VNC_PIXELFORMAT_32BPP_RGB888 VNC_PIXELFORMAT_16BPP_RGB565 VNC_PIXELFORMAT_8BPP_RGB332);


sub buffer2hex($) {
	my ($t) = @_ ;

	my $result = '';

	my @b = split (//, $t) ;

	foreach (@b) {
		$result .= sprintf("%02X ", ord($_)) ;
	}

	return $result ;
}

########## 
#
# helper function to build the PIXELFORMAT binary struct (please see VNC documentation
#
sub BUILD_PIXEL_FORMAT {
	my ($bits_per_pixel, 
		$depth ,
		$big_endian,
		$true_color,
		$red_max,$green_max,$blue_max,
		$red_shift,$green_shift, $blue_shift) = @_ ;

	my $buffer = pack ("CCCCnnnCCCCn", $bits_per_pixel, 
		$depth ,
		$big_endian,
		$true_color,
		$red_max,$green_max,$blue_max,
		$red_shift,$green_shift, $blue_shift ) ;

	return $buffer;
}

sub VNC_PIXELFORMAT_32BPP_RGB888 {
	return BUILD_PIXEL_FORMAT(32,24,1,1,255,255,255,16,8,0);
}

sub VNC_PIXELFORMAT_16BPP_RGB565 {
	return BUILD_PIXEL_FORMAT(16,16,1,1,31,63,31,11,5,0);
}

sub VNC_PIXELFORMAT_8BPP_RGB332 {
	return BUILD_PIXEL_FORMAT(8,8,1,1,7,7,3,0,3,6);
}

#######
# Constructs a new VNC Codec object
#
sub new {
	my $proto = shift;
	my $class = $proto;

	my $self  = {};
	bless ($self, $class);

	#use default PIXELFORMAT of 32bpp, RGB888
	$self->{RED_MAX}		= 255 ;
	$self->{GREEN_MAX}		= 255 ;
	$self->{BLUE_MAX}		= 255 ;
	$self->{RED_SHIFT}		= 16;
	$self->{GREEN_SHIFT}	= 8 ;
	$self->{BLUE_SHIFT}		= 0 ;
	$self->{BITS_PER_PIXEL} = 32; 
	$self->{DEPTH}			= 24 ;
	$self->{TRUE_COLOR}		= 1 ;
	$self->{BIG_ENDIAN}		= 1 ;

	
	#This is used for the internal representation o RGB data
	$self->{INTERNAL_RED_MAX} = 255 ;
	$self->{INTERNAL_GREEN_MAX} = 255 ;
	$self->{INTERNAL_BLUE_MAX} = 255 ;

	return $self;
}

sub set_pixel_format {
	my ($self, $bits_per_pixel, 
		$depth ,
		$big_endian,
		$true_color,
		$red_max,$green_max,$blue_max,
		$red_shift,$green_shift, $blue_shift) = @_ ;

	$self->{RED_MAX}		= $red_max ;
	$self->{GREEN_MAX}		= $green_max ;
	$self->{BLUE_MAX}		= $blue_max ;
	$self->{RED_SHIFT}		= $red_shift;
	$self->{GREEN_SHIFT}	= $green_shift ;
	$self->{BLUE_SHIFT}		= $blue_shift ;
	$self->{BITS_PER_PIXEL} = $bits_per_pixel; 
	$self->{DEPTH}			= $depth ;
	$self->{TRUE_COLOR}		= $true_color ;
	$self->{BIG_ENDIAN}		= $big_endian ;

	return 1;
}


#############################
#
# This function builds a raw pixel, according to the specified format (bits/pixel,and max/shift values)
# It returns a raw (binary) scalar with the encoded pixel
#
sub encode_pixel {
	my ($self,$r,$g,$b) = @_ ;

	warn "VNC::Codec::encode_pixel called with self=undef" unless (defined $self);
	warn "VNC::Codec::encode_pixel called with r/g/b=undef" unless (defined $r and defined $g and defined $b);

	my $red_max = $self->{RED_MAX};
	my $green_max = $self->{GREEN_MAX};
	my $blue_max = $self->{BLUE_MAX};

	my $red_shift = $self->{RED_SHIFT};
	my $green_shift = $self->{GREEN_SHIFT};
	my $blue_shift = $self->{BLUE_SHIFT};

	my $bits_per_pixel = $self->{BITS_PER_PIXEL};

	my $z = ($r * ($red_max))/$self->{INTERNAL_RED_MAX} << $red_shift ;
	$z +=   ($g * ($green_max))/$self->{INTERNAL_GREEN_MAX} << $green_shift;
	$z +=   ($b * ($blue_max))/$self->{INTERNAL_BLUE_MAX} << $blue_shift;

	my $raw;

	if ($bits_per_pixel==8) {
		$raw .= pack("C",$z) ;
	} elsif ($bits_per_pixel==16) {
		$raw .= pack("S",$z) ;
	} elsif ($bits_per_pixel==32) {
		$raw .= pack("i",$z) ;
	}

#	print "encode_pixel($r,$g,$b) = ",buffer2hex($raw),"\n";

	return $raw;
}

#############################
#
# This function builds a RAW encoded rectangle, with a singe RGB color
# It returns a raw (binary) scalar with the encoded rect data
#
sub encode_raw_rect {
	my ($self,$raw_pixel,$x,$y,$w,$h) = @_ ;

	my $i;
	my $j;
	my $raw ;
	
	for ($i=0;$i<$h;$i++) {
		for ($j=0;$j<$w;$j++) {
			$raw .= $raw_pixel; 
		}
	}

	my $buffer = pack("nnnnN", $x,$y,$w,$h,0 ) . $raw ;
	
#	print "encode_raw_rect = ",buffer2hex($buffer),"\n";
	return $buffer;
}

#############################
#
# This function builds an RRE sub-rect
# It returns a raw (binary) scalar with the encoded subrect data
#
sub encode_rre_subrect {
	my ($self,$raw_pixel,$x,$y,$w,$h) = @_ ;

	warn "VNC::Codec::encode_rre_subrect called with self=undef" unless (defined $self);
	warn "VNC::Codec::encode_rre_subrect called with raw_pixel=undef" unless (defined $raw_pixel);
	warn "VNC::Codec::encode_rre_subrect called with x/y/w/h=undef" unless (defined $x and defined $y and defined $w and defined $h);
	
	return $raw_pixel . pack("nnnn",$x,$y,$w,$h);
}

#############################
#
# This function builds an RRE sub-rect
# It returns a raw (binary) scalar with the encoded subrect data
#
sub encode_rre {
	my ($self,$raw_background_pixel,$x,$y,$w,$h,@subrects) = @_ ;

	warn "VNC::Codec::encode_rre called with self=undef" unless (defined $self);
	warn "VNC::Codec::encode_rre called with x/y/w/h=undef" unless (defined $x and defined $y and defined $w and defined $h);
	warn "VNC::Codec::encode_rre called with raw_background_pixel=undef" unless (defined $raw_background_pixel);

	my $count = scalar @subrects;
	
	return pack("nnnnNN",$x,$y,$w,$h,2,$count) . $raw_background_pixel . join('',@subrects); # 2 = RRE encoding
}

sub encode_framebuffer_update {
	my ($self,@rectangles) = @_ ;

	my $count = scalar @rectangles;
	my $buffer = pack("CCn",0,0,$count) . join('',@rectangles) ;

	return $buffer;
}

1;