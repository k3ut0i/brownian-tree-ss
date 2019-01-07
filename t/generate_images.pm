#!/usr/bin/perl
use strict;
use warnings;
use Cwd qw(cwd);

use constant ISOLATED_POINTS_EXE => "./isolated_points.bin";
use constant COCS_EXE            => "./center_origin_circle_seeded.bin";
use constant TGSBT_EXE           => "./center_seeded.bin";
use constant SES_EXE             => "./square_ends_seeded.bin";
use constant NUM_SAMPLES         => 10;
use constant SEED_MAX            => 1000;
use constant SIZE => 400;

sub isolated_points{
  my ($width, $height, $seed, $npart, $nseeds, $outfile) = @_;
  system(ISOLATED_POINTS_EXE, $width, $height, $seed,
	 $npart, $nseeds, $outfile) == 0
    or die "isolated_points failed: $?";
}

sub generate_ip{
  my $seed = 0;
  select()->flush();
  foreach (1 .. NUM_SAMPLES) {
    $seed = int (rand SEED_MAX);
    isolated_points(SIZE, SIZE, $seed, 5000, 4, "isolated_${seed}.pbm");
    print ".";
  }
  print "Isolated Images ", NUM_SAMPLES, "\n";
}

sub generate_center_multiple_seeds{
  my $width = 200;
  my $height = 200;
  my $num_samples = 10;
  my $num_particles = 4000;
  ($width, $height, $num_samples, $num_particles) = @_;
  foreach (1 .. $num_samples) {
    my $seed = int(rand SEED_MAX);
    system(TGSBT_EXE, $height, $width, $num_particles, $seed) == 0
      or die "center seeds binary failed: $?";
  }
}

1;
