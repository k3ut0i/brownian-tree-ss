#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "test_misc.h"
#include "brownian_tree.h"

#define half(x) x % 2 ? (x-1)/2 : x/2


void center_seeded(struct brownian_tree* t, unsigned long n){
  assert(t);
  unsigned long cx = half(t->x);
  unsigned long cy = half(t->y);
  bt_new_seed_at(t, cx, cy);
  for(unsigned long i = 0; i < n; i++){
    while(!bt_new_random_particle(t)){}
  }
}

void handle_test(char* str, struct brownian_tree* t){
  if(strcmp(str, "center_seeded") == 0)
    center_seeded(t, config.npart);
}

int main(int argc, char *argv[])
{
  if(populate_config(argc, argv) != WIDTH+HEIGHT+RSEED+OUTFILE+NPART+TEST_TYPE){
    print_banner(argv[0]);
    exit(EXIT_FAILURE);
  }
  struct brownian_tree* t = bt_init(config.width,
				    config.height,
				    config.rseed);
  /* Handle arguments that define type of output needed. */
  center_seeded(t, config.npart);
  bt_dump_all_info_to_file(t, config.outfile);
  bt_destroy(t);
  return 0;
}

