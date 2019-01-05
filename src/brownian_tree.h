#ifndef BROWNIAN_TREE_H
#define BROWNIAN_TREE_H

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef DEBUG
#define DBG(str, ...) do{fprintf(stdout, str, __VA_ARGS__);}while(0)
#else
#define DBG(str, ...)
#endif


typedef long ul;

enum node_type{
  EMPTY = 0,
  SEED,
  PARTICLE,
};

struct node{
  enum node_type type;
  int attributes;
  ul depth;

  unsigned long long steps;
  ul from_x;
  ul from_y;
  struct node * connected_seed;

};

struct brownian_tree{
  ul x, y;              /* Dimensions for the tree. */
  struct node * buffer; /* Buffer for node values. */
  unsigned int rseed;
  char * state;         /* State of psuedo-random generator for reproducible generation. */
  unsigned long num_particles;
  unsigned long long out_of_bounds;
  unsigned long long total_steps;
  unsigned long long successful_steps;

};

typedef ul (*rp_gen)(ul, ul); 

struct brownian_tree * bt_init(const ul max_x, const ul max_y, unsigned int rseed);
void bt_destroy(struct brownian_tree * t);
void bt_new_seed_at(struct brownian_tree * t, const ul x, const ul y);

/* Deprecated: Use from_to functions that do not change state. */
bool bt_new_particle_at(struct brownian_tree * t, ul x, ul y)
  __attribute__ ((deprecated));
bool bt_new_random_particle(struct brownian_tree * t)
  __attribute__ ((deprecated));

/* TODO: rewrite bt_new_particle_at as a stateless fn. */
bool bt_new_particle_from_to(struct brownian_tree * t,
			     const ul from_x, const ul from_y,
			     const ul * to_x, const ul * to_y);

/* TODO: Testing require for these functions. */
ul bt_npart_from(struct brownian_tree* t, ul* xy, ul n);
void bt_npart(struct brownian_tree* t, rp_gen f, ul n);

/* TODO: Threaded functions. */
int bt_npart_threaded(struct brownian_tree* t, rp_gen f, ul n);


bool on_tree_p(struct brownian_tree*, ul, ul);
ul touch_tree(struct brownian_tree*, ul, ul);

void bt_dump_to_pbm(struct brownian_tree * t, FILE * fp);
void bt_dump_to_pbm_file(struct brownian_tree * t, const char * filename);

void bt_dump_all_info(struct brownian_tree* t, FILE* fp);
void bt_dump_all_info_to_file(struct brownian_tree* t, const char* filename);

#endif
