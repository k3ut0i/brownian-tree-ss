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

  fprintf(stdout, "Brownian Tree: w:%ld, h:%ld, Init:(%ld,%ld)",
	  width, height, (long)floor(width/2), (long)floor(height/2));
  struct brownian_tree * t = bt_init(width, height, rseed);
  /* Single seed at the centre. */
  bt_new_seed_at(t, floor((width-1)/2), floor((height-1)/2));

  /* Particles originate from the borders. */
  for(unsigned long i = 0; i < npart; i++){
    unsigned int side = floor(4*random()/RAND_MAX);
    unsigned long x,y;
    do{
      switch(side){
      case 0: /* Left */
	x = 0; y = floor(height*random()/RAND_MAX);
	break;
      case 1: /* Top */
	x = floor(width*random()/RAND_MAX); y = 0;
	break;
      case 2: /* Right */
	x = width - 1; y = floor(height*random()/RAND_MAX);
	break;
      case 3:
	x = floor(width*random()/RAND_MAX); y = height - 1;
	break;
      default:
	fprintf(stderr, "Invalid switch case\n");
	return(-1);
	break;
      }
    } while(!bt_new_particle_at(t, x, y));
  }
  bt_dump_to_pbm_file(t, outfile);
  bt_destroy(t);
  return 0;
}
