#include "brownian_tree.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <err.h>
#include <math.h>

int main(int argc, char *argv[])
{
  if(argc != 4)
    err(-1, "Usage: %s <square_side_length> <num_particles> <seed>", argv[0]);

  const unsigned long side_length = strtoul(argv[1], NULL, 0);
  const unsigned long npart       = strtoul(argv[2], NULL, 0);
  const unsigned int rseed        = strtoul(argv[3], NULL, 0);
  const char * file_prefix = "cocs";
  const char * file_postfix = ".pbm";

  int str_size =
    strlen(file_prefix) + 1 + /* Underscore */
    strlen(argv[1]) + 1 + /* Underscore */
    strlen(argv[2]) + 1 + /* Underscore */
    strlen(argv[3]) +
    strlen(file_postfix) + 1; /* Null terminated */
  char * outfile = malloc(str_size);
  snprintf(outfile, str_size, "%s_%s_%s_%s%s",
	   file_prefix, argv[1], argv[2], argv[3], file_postfix);
  fprintf(stdout, "Brownian tree with seeded circle and points from inner"
	  "concentric circle\n"
	  "Size: %ldX %ld Num of Particles: %ld\n",
	  side_length, side_length, npart);

  struct brownian_tree* t = bt_init(side_length, side_length, rseed);
  const unsigned long radius =
    side_length % 2 == 0 ? side_length/2 : (side_length-1)/2;
  double distance = 0.0;
  for(unsigned long i = 0; i < side_length; i++)
    for(unsigned long j = 0U; j < side_length; j++){
      /* Integer sqrt can solve inefficient sqrt, floor functions both. */
      distance = sqrt((i-radius)*(i-radius)+(j-radius)*(j-radius));
      if(floor(distance) == radius)
	bt_new_seed_at(t, i, j);
    }
  for(unsigned long i = 0; i < npart; i++){
    if(touch_tree(t, radius, radius) != 0){
      fprintf(stdout, "\nTree grew to the center, terminating at %ld particles\n",
	      i);
      goto cleanup;
    }else{
      while(!bt_new_particle_at(t, radius, radius)){
	fprintf(stderr, "\rTrying particle %ld", i);	
      }
      fflush(stderr);
    }
  }

 cleanup:
  bt_dump_to_pbm_file(t, outfile);
  free(outfile);
  bt_destroy(t);
  return 0;
}
