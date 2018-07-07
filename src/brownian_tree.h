#ifndef BROWNIAN_TREE_H
#define BROWNIAN_TREE_H

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

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

bool bt_new_particle_at(struct brownian_tree * t, ul x, ul y);
bool bt_new_random_particle(struct brownian_tree * t);

/* TODO: Testing require for these functions. */
ul bt_npart_from(struct brownian_tree* t, ul* xy, ul n);
void bt_npart(struct brownian_tree* t, rp_gen f, ul n);



bool on_tree_p(struct brownian_tree*, ul, ul);

void bt_dump_to_pbm(struct brownian_tree * t, FILE * fp);
void bt_dump_to_pbm_file(struct brownian_tree * t, const char * filename);

void bt_draw_sample(const char * filename);
#endif
