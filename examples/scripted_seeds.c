#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <libguile.h>

#include "brownian_tree.h"

#define getul(str) strtoul(str, NULL, 0)

typedef struct _point{
  unsigned long x, y;
} point;

void get_point(point* p, SCM ps){

  SCM xs = scm_car(ps);
  SCM ys = scm_cdr(ps);
  unsigned long x = scm_to_ulong(xs);
  unsigned long y = scm_to_ulong(ys);
  p->x = x;
  p->y = y;
  return;
}

int main(int argc, char *argv[])
{
  if(argc != 6){
    fprintf(stderr, "Usage: %s <size> <seed> <npart> <outfile> <scriptfile>", argv[0]);
    exit(EXIT_FAILURE);
  }
  unsigned long size = getul(argv[1]);
  unsigned long seed = getul(argv[2]);
  unsigned long npart = getul(argv[3]);
  const char* outfile = argv[4];
  const char* scriptfile = argv[5];

  struct brownian_tree* t = bt_init(size, size, seed);

  /* Seeding */
  SCM ps = SCM_BOOL_F;
  scm_init_guile();
  scm_c_primitive_load(scriptfile);
  SCM gen_symbol = scm_c_lookup("rgn-generator");
  SCM gen        = scm_variable_ref(gen_symbol);
  SCM func       = scm_call_1(gen, scm_from_ulong(size));
  point* p = malloc(sizeof (point));
  while((ps = scm_call_0(func))!= SCM_BOOL_F){
    get_point(p, ps);
    bt_new_seed_at(t, p->x, p->y);
  }
  free(p);
  /* Particles */
  for(unsigned long i = 0; i < npart; i++){
    while(!bt_new_random_particle(t)){
      DBG("\rTrying particle %ld", i);
    }
  }
  bt_dump_to_pbm_file(t, outfile);
  bt_destroy(t);
  return 0;
}
