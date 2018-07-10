#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <libguile.h>

#include "brownian_tree.h"
#include "misc.h"

int main(int argc, char *argv[])
{
  handle_args(argc, argv);
  struct brownian_tree* t = bt_init(size, size, rseed);
  unsigned long center = size % 2 ? (size-1)/2 : size/2;
  bt_new_seed_at(t, center, center);
  scm_c_primitive_load(scriptfile);
  SCM gen = scm_variable_ref(scm_c_lookup("generator"));
  SCM func = scm_call_1(gen, scm_from_ulong(size));

  point* p = malloc(sizeof(point));
  for(unsigned long i = 0; i < npart; i++){
    get_point(p, scm_call_0(func));
    bt_new_particle_at(t, p->x, p->y);
  }
  bt_dump_to_pbm_file(t, outfile);
  bt_destroy(t);
  return 0;
}

