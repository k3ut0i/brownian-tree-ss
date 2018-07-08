#!/usr/bin/perl
use strict;
use warnings;
use Cwd qw(cwd);

use constant ISOLATED_POINTS_EXE => "./isolated_points.bin";
use constant COCS_EXE            => "center_origin_circle_seeded.bin";
use constant TGSBT_EXE           => "center_seeded.bin";
use constant SIZE => 400;

sub isolated_points{
  my ($width, $height, $seed, $npart, $nseeds, $outfile) = @_;
  system(ISOLATED_POINTS_EXE, $width, $height, $seed,
	 $npart, $nseeds, $outfile) == 0
    or die "isolated_points failed: $?";
}

sub generate_ip{
  isolated_points(SIZE, SIZE, 1, 5000, 4, "isolated.pbm");
}

print cwd, "\n";
generate_ip();
