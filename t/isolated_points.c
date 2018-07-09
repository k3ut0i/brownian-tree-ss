#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

#include "brownian_tree.h"

int main(int argc, char *argv[])
{
  if(argc != 7)
    err(-1, "Usage: %s <width> <height> <seed> "
	"<nparticles> <nseeds> <outfile>", argv[0]);
  unsigned long width  = strtoul(argv[1], NULL, 0);
  unsigned long height = strtoul(argv[2], NULL, 0);
  unsigned int seed    = strtoul(argv[3], NULL, 0);
  unsigned long npart  = strtoul(argv[4], NULL, 0);
  unsigned long nseeds = strtoul(argv[5], NULL, 0);
  const char * outfile = argv[6];

  struct brownian_tree* t = bt_init(width, height, seed);
  unsigned long x = 0, y = 0;
  for(unsigned long i = 0; i < nseeds; i++){
    x = width*random()/RAND_MAX;
    y = height*random()/RAND_MAX;
    bt_new_seed_at(t, x, y);
  }

  for(unsigned long i = 0; i < npart; i++){
    while(!bt_new_random_particle(t)){
      DBG("\rTrying particle %ld", i);
    }
  }
  bt_dump_to_pbm_file(t, outfile);
  bt_destroy(t);
  return 0;
}
