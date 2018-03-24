#include <stdio.h>
#include <stdlib.h>
#include <err.h>
#include <math.h>
#include "brownian_tree.h"

int main(int argc, char *argv[])
{
  if(argc != 5)
    err(-1, "Usage %s: <height> <width> <num_particles> <out file>", argv[0]);
  const unsigned long height = strtoul(argv[1], NULL, 0);
  const unsigned long width  = strtoul(argv[2], NULL, 0);
  const unsigned long npart  = strtoul(argv[3], NULL, 0);
  const char * outfile = argv[4];
  const unsigned int rseed = 0;

  struct brownian_tree * t = bt_init(width, height, rseed);
  bt_new_seed(t, floor((width-1)/2), floor((height-1)/2));
  for(unsigned long i = 0; i < npart; i++)
    bt_new_particle(t, 0, 0);

  bt_dump_to_pbm_file(t, outfile);
  bt_destroy(t);
  return 0;
}
