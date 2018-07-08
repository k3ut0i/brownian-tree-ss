#include <stdio.h>
#include <err.h>
#include <math.h>
#include "brownian_tree.h"

int main(int argc, char *argv[])
{
  if (argc != 5) {
    fprintf(stderr, "Usage: %s <size> <npart> <seed> <outfile>", argv[0]);
    exit(EXIT_FAILURE);
  }
  unsigned long size = strtoul(argv[1], NULL, 0);
  unsigned long npart = strtoul(argv[2], NULL, 0);
  unsigned long seed = strtoul(argv[3], NULL, 0);
  const char * outfile = argv[4];

  struct brownian_tree* t = bt_init(size, size, seed);
  unsigned long p1 = size/4;
  unsigned long p3 = size - p1;
  bt_new_seed_at(t, p1, p1);
  bt_new_seed_at(t, p1, p3);
  bt_new_seed_at(t, p3, p3);
  bt_new_seed_at(t, p3, p1);
  for(unsigned long i = 0; i < npart; i++){
    while(!bt_new_random_particle(t)){
      fprintf(stderr, "\rTrying particle %ld", i);
    }
  }
  bt_dump_to_pbm_file(t, outfile);
  bt_destroy(t);
  return 0;
}
