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

void init_bt_type(void)
{
  SCM name, slots;
  scm_t_struct_finalize finalizer;

  name = scm_from_utf8_symbol("brownian_tree");
  slots = scm_list_1(scm_from_utf8_symbol("data"));
  finalizer = NULL;
  brownian_tree_type = scm_make_foreign_object_type(name, slots, finalizer);
}

SCM_DEFINE(bt_init_wrapper, "bt-init", 3, 0, 0,
	   (SCM width, SCM height, SCM seed),
	   "Initial Brownian Tree structure.")
{
  
}

SCM_DEFINE(bt_new_seed_at_wrapper, "bt-new-seed-at", 3, 0, 0,
	   (SCM tree, SCM x, SCM y),
	   "Introduce new seed at (X,Y) on the TREE.")
{

}

void init_bt_lib()
{
#ifndef SCM_MAGIC_SNARFER
#include "bt-guile-bindings.x"
#endif

}
