#include "brownian_tree.h"

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <error.h>
#include <errno.h>

#define in_boundsp(n, min, max) n >= min && n < max
#define min(a,b) a>b?b:a
static ul touch_tree(struct brownian_tree * t, ul x, ul y)
{
  ul ret = 0;
  for(int i = -1; i<=1; i++)
    for(int j = -1; j<=1; j++)
      if(i && j) {
	struct node* n = t->buffer + x*t->y + y;
	switch (n->type) {
	case SEED:
	  return 1;
	  break;
	case EMPTY:
	  break;
	case PARTICLE:
	  ret = min(ret, n->depth+1);
	  break;
	}
      }
  return ret;
}

struct brownian_tree *
bt_init(const ul max_x, const ul max_y, unsigned int rseed)
{
  struct brownian_tree * t = malloc(sizeof *t);
  t->x = max_x;
  t->y = max_y;
  t->buffer = malloc(max_x * max_y * (sizeof (struct node)));
  t->rseed = rseed;

#ifdef STATS
  t->out_of_bounds    = 0;
  t->total_steps      = 0;
  t->successful_steps = 0;
#endif // STATS
  
  for(ul i = 0; i < max_x; i++)
    for(ul j = 0; j < max_y; j++){
      struct node * n = t->buffer + i*max_y +j;
      n->type           = EMPTY;
      n->depth          = -1;
      n->attributes     = 0;
#ifdef STATS
      n->connected_seed = NULL;
      n->steps          = 0;
      n->from_x         = 0;
      n->from_y         = 0;
#endif // STATS
    }
  srandom(t->rseed);
  return t;
}


void bt_destroy(struct brownian_tree * t){
  free(t->buffer);
  free(t);
}

bool bt_new_particle(struct brownian_tree * t, ul x, ul y)
{
  long new_x = x;
  long new_y = y;
  int rstep;
  ul new_depth;
  unsigned long long nsteps = 0;
  while (in_boundsp(new_x, 0, t->x) && in_boundsp(new_y, 0, t->y)) {
    rstep = floor(8*random()/RAND_MAX);
    assert(0 <= rstep && rstep < 8);
    switch(rstep){
    case 0: /* East. */
      new_x++;
      break;
    case 1: /* North East. */
      new_x++;
      new_y++;
      break;
    case 2: /* North. */
      new_y++;
      break;
    case 3: /* North West. */
      new_x--;
      new_y++;
      break;
    case 4: /* West. */
      new_x--;
      break;
    case 5: /* South West. */
      new_x--;
      new_y--;
      break;
    case 6: /* South. */
      new_y--;
      break;
    case 7: /* South East. */
      new_x++;
      new_y--;
      break;
    }
    new_depth = touch_tree(t, new_x, new_y);
    if(new_depth){
      struct node* new_node = t->buffer + new_x * t->y + new_y;
      new_node->type = PARTICLE;
      new_node->depth = new_depth;
#ifdef STATS
      new_node->steps = nsteps;
      new_node->from_x = x;
      new_node->from_y = y;
      /* Returning both connected node, depth is cumbersome.
Including touch functionality here breaks abstraction. Do i need connection link?*/
      
#endif // STATS
      return true;
    }
    nsteps++;
  }
  return false;
}

void bt_new_seed(struct brownian_tree * t, const ul x, const ul y)
{
  struct node * seed_node = t->buffer + x * t->y + y;
  seed_node->type = SEED;
  seed_node->depth = 0;
}

void bt_dump_to_pbm_file(struct brownian_tree * t, const char * filename)
{
  assert(t);
  FILE *fp = fopen(filename, "w");
  if(!fp) error(-1, errno, "In %s", __func__);
  
  /* Print Image header. */
  fprintf(fp, "P1\n");
  fprintf(fp, "%ld %ld\n", t->y, t->x);
  
  struct node * cursor = t->buffer;
  for(ul i = 0; i< t->x; i++){
    for(ul j = 0; j < t->y; j++){
      cursor = t->buffer + t->y * i + j;
      fprintf(fp, "%d ", cursor->type == EMPTY ? 0 : 1);
    }
    fputc('\n', fp);
  }
  fclose(fp);
}
