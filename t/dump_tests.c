#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

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

int main(int argc, char *argv[])
{
  assert(populate_config(argc, argv) == 0b10111);
  assert(validate_config());
  struct brownian_tree* t = bt_init(config.width,
				    config.height,
				    config.rseed);
  bt_dump_to_pbm_file(t, config.outfile);
  bt_destroy(t);
  return 0;
}

