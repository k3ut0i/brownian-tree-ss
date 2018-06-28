#include <libguile.h>
#include "brownian_tree.h"

static SCM node_type;

void init_node_type(void)
{
  SCM name, slots;
  scm_t_struct_finalize finalizer;

  name = scm_from_utf8_symbol("node");
  slots = scm_list_1(scm_from_utf8_symbol("data"));
  finalizer = NULL;
  node_type = scm_make_foreign_object_type(name, slots, finalizer);
}

static SCM brownian_tree_type;
static void finalize_brownian_tree(SCM bt)
{
  bt_destroy(scm_foreign_object_ref(bt,0));
}

static void init_bt_type(void)
{
  SCM name, slots;
  scm_t_struct_finalize finalizer;

  name = scm_from_utf8_symbol("brownian_tree");
  slots = scm_list_1(scm_from_utf8_symbol("data"));
  finalizer = finalize_brownian_tree;
  brownian_tree_type = scm_make_foreign_object_type(name, slots, finalizer);
}


SCM_DEFINE(make_brownian_tree, "bt-new", 3, 0, 0,
	   (SCM width, SCM height, SCM seed),
	   "Create a new Empty Tree of WIDTH x HEIGHT size, random SEED.")
{
  ul w = scm_to_ulong(width);
  ul h = scm_to_ulong(height);
  unsigned int s = scm_to_uint(seed);
  struct brownian_tree * t = bt_init(w, h, s);
  return scm_make_foreign_object_1(brownian_tree_type, t);
}

SCM_DEFINE(bt_new_seed_at_wrapper, "bt-new-seed-at", 3, 0, 0,
	   (SCM tree, SCM x, SCM y),
	   "Introduce new seed at (X,Y) on the TREE.")
{
  return SCM_BOOL_F;
}

SCM_DEFINE(bt_new_particle_at_wrapper, "bt-new-particle-at", 3, 0, 0,
	   (SCM tree, SCM x, SCM y),
	   "Introduce a new particle at (X,Y) to the TREE>")
{
  return SCM_BOOL_F;
}

SCM_DEFINE(on_tree_p_wrapper, "bt-on-tree?", 3, 0, 0,
	   (SCM tree, SCM x, SCM y),
	   "Is the point (X,Y) on the TREE ?")
{
  scm_display(scm_from_locale_string("hey howdy?\n"),
	      scm_current_output_port());
  return SCM_BOOL_F;
}


void init_bt_lib()
{
#ifndef SCM_MAGIC_SNARFER
#include "bt-guile-bindings.x"
#endif

}
