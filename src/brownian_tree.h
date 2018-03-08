#ifndef BROWNIAN_TREE_H
#define BROWNIAN_TREE_H
#include <stdbool.h>
#include <stdlib.h>

typedef unsigned long ul;

enum node_type{
  EMPTY,
  SEED,
  PARTICLE,
};

struct node{
  enum node_type type;
  int attributes;
  ul depth;
#ifdef STATS
  unsigned long long steps;
  ul from_x;
  ul from_y;
  struct node * connected_seed;
#endif
};

struct brownian_tree{
  ul x, y;              /* Dimensions for the tree. */
  struct node * buffer; /* Buffer for node values. */
  unsigned int seed;
  char * state;         /* State of psuedo-random generator for reproducible generation. */
#ifdef STATS
  unsigned long long out_of_bounds;
  unsigned long long total_steps;
  unsigned long long successful_steps;
#endif // STATS  
};


struct brownian_tree * bt_init(const ul max_x, const ul max_y, unsigned int seed);
void bt_new_seed(struct brownian_tree * t, const ul x, const ul y);
bool bt_new_particle(struct brownian_tree * t, ul x, ul y);
void bt_destroy(struct brownian_tree * t);

#endif
