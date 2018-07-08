#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>
#include <math.h>
#include "brownian_tree.h"

int main(int argc, char *argv[])
{
  if(argc != 5)
    err(-1, "Usage %s: <height> <width> <num_particles> <seed>", argv[0]);
  const unsigned long height = strtoul(argv[1], NULL, 0);
  const unsigned long width  = strtoul(argv[2], NULL, 0);
  const unsigned long npart  = strtoul(argv[3], NULL, 0);
  const unsigned int rseed = strtoul(argv[4], NULL, 0);
  const char * file_prefix = "sample";
  const char * file_postfix = ".pbm";
  int str_size = (strlen(file_prefix) + 1 + /* Underscore */
		  strlen(argv[1]) + 1 + /*X*/
		  strlen(argv[2]) + 1 + /* Underscore */
		  strlen(argv[3]) + 1 + /* Underscrore */
		  strlen(argv[4]) +
		  strlen(file_postfix) + 1);
  char * outfile = malloc(str_size);
  snprintf(outfile, str_size, "%s_%sx%s_%s_%s%s",
	   file_prefix,
	   argv[1],
	   argv[2],
	   argv[3],
	   argv[4],
	   file_postfix);


  fprintf(stdout, "Brownian Tree: w:%ld, h:%ld, Init:(%ld,%ld) to %s\n",
	  width, height, (long)floor(width/2), (long)floor(height/2), outfile);
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
  free(outfile);
  bt_destroy(t);
  return 0;
}
