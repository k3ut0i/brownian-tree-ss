#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <libguile.h>

#include "brownian_tree.h" /* bt_* */
#include "misc.h" /* handle_args, get_point, point. */

int main(int argc, char *argv[])
{

  handle_args(argc, argv);
  struct brownian_tree* t = bt_init(size, size, rseed);

  /* Seeding */
  SCM ps = SCM_BOOL_F;
  scm_init_guile();
  scm_c_primitive_load(scriptfile);
  SCM gen_symbol = scm_c_lookup("circle-generator");
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
