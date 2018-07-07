#!/usr/bin/perl
use strict;
use warnings;

use Cwd qw(getcwd);
use Test::More;
use File::Compare;

my $width = 100;
my $height = 100;
my $num = 3000;

my $test_exe = "test-generate-simple-bt";
ok(-e $test_exe, "Test executible file");

if (-e $test_exe) {
  for (1 .. 10) {
    my $file1 = "sample_${width}X${height}_${num}_$_.pbm";
    my $file2 = "sample_f${width}X${height}_${num}_$_.pbm";
    ok(-e $file1, "$file1 found");
    ok(-e $file2, "$file2 found");
    ok(compare($file1, $file2), "Comparing $file1 and $file2")
      if -e $file1 && -e $file2;
  }
}
done_testing();
