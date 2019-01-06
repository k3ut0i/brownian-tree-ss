#include "brownian_tree.h"

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <error.h>
#include <errno.h>
#include <err.h>

#define in_boundsp(n, min, max) n >= min && n < max
#define constrain(min, x, max) x < min ? min : (x > max ? max : x)
#define min(a,b) a>b?b:a

bool on_tree_p(struct brownian_tree * t, ul x, ul y)
{
  struct node * n = t->buffer + x*t->y + y;
  if(n->type == EMPTY)
    return false;
  else
    return true;
}

ul touch_tree(struct brownian_tree * t, ul x, ul y)
{
  if(!(in_boundsp(x, 0, t->x) && in_boundsp(y, 0, t->y))) return 0;
  ul ret = 0;
  for(int i = -1; i<=1; i++)
    for(int j = -1; j<=1; j++)
      if(i || j) {
	/* TODO: redoing out of bounds co-ordinates, 
	   should be more logical and skip it entirely.*/
	ul x_new = constrain(0, x+i, t->x-1);
	ul y_new = constrain(0, y+j, t->y-1);
	struct node* n = t->buffer + x_new*t->y + y_new;
	switch (n->type) {
	case SEED:
	  ret = 1;
	  break;
	case EMPTY:
	  break;
	case PARTICLE:
	  ret = ret == 0 ? n->depth+1 : min(ret, (n->depth+1));
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
  t->num_particles = 0;
  
  t->out_of_bounds    = 0;
  t->total_steps      = 0;
  t->successful_steps = 0;

  
  for(ul i = 0; i < max_x; i++)
    for(ul j = 0; j < max_y; j++){
      struct node * n = t->buffer + i*max_y +j;
      n->type           = EMPTY;
      n->depth          = 0;
      n->attributes     = 0;
      n->connected_seed = NULL;
      n->steps          = 0;
      n->from_x         = 0;
      n->from_y         = 0;
    }
  srandom(t->rseed);
  return t;
}


void bt_destroy(struct brownian_tree * t){
  free(t->buffer);
  free(t);
}

/* FIXME: Num of steps and out-of-bounds has some bug */
bool bt_new_particle_at(struct brownian_tree * t, ul x, ul y)
{
  if(on_tree_p(t, x, y)) return false;
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
      
      new_node->steps = nsteps;
      new_node->from_x = x;
      new_node->from_y = y;

      t->num_particles++;
      t->successful_steps += nsteps;
      DBG("New node[%ld] at %ld, %ld from %ld, %ld\n",
	  t->num_particles, new_x, new_y, x, y);
      /* Returning both connected node, depth is cumbersome.
Including touch functionality here breaks abstraction. Do i need connection link?*/
      return true;
    }else{
      t->out_of_bounds++;
    }
    t->total_steps++;
    nsteps++;
  }
  return false;
}

void bt_new_seed_at(struct brownian_tree * t, const ul x, const ul y)
{
  if (in_boundsp(x, 0, t->x) && in_boundsp(y, 0, t->y)){
    struct node * seed_node = t->buffer + x * t->y + y;
    seed_node->type = SEED;
    seed_node->depth = 0;
  }else{
    err(-1, "Attempt to set seed, invalid bounds (%ld, %ld) (%ld, %ld).",
	x, y, t->x, t->y);
  }
}

bool bt_new_random_particle(struct brownian_tree* t)
{
  unsigned long x = t->x * random()/RAND_MAX;
  unsigned long y = t->y * random()/RAND_MAX;
  return bt_new_particle_at(t, x, y);
}

void bt_dump_to_pbm(struct brownian_tree * t, FILE * fp)
{
  
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

}

void bt_dump_to_pbm_file(struct brownian_tree * t, const char * filename)
{
  assert(t);
  FILE *fp = fopen(filename, "w");
  if(!fp) error(-1, errno, "In %s", __func__);
  bt_dump_to_pbm(t, fp);
  fclose(fp);
}

void bt_dump_all_info(struct brownian_tree * t, FILE* fp)
{
  assert(t);
  /* TODO: Dump all info about the tree to an output stream. */
  struct node * cursor = t->buffer;
  fprintf(fp, "%ld\t%ld\n", t->x, t->y);
  for(ul i = 0; i< t->x; i++){
    for(ul j = 0; j < t->y; j++){
      cursor = t->buffer + t->y * i + j;
      fprintf(fp, "[%ld,%ld]\t%d\t%d\t%ld\t%lld\t[%ld,%ld]\n"
	      , i, j,
	      cursor->type,
	      cursor->attributes, cursor->depth, cursor->steps,
	      cursor->from_x, cursor->from_y);
    }
  }
}

void bt_dump_all_info_to_file(struct brownian_tree* t, const char* filename)
{
  assert(t);
  FILE *fp = fopen(filename, "w");
  if(!fp) error(-1, errno, "In %s", __func__);
  bt_dump_all_info(t, fp);
  fclose(fp);
}

ul bt_npart_from(struct brownian_tree* t, ul* xy_points, ul size)
{
  ul ret = 0;
  ul x = 0, y = 0;
  for(ul i = 0; i < size; i++){
    x = *(xy_points++);
    y = *(xy_points++);
    if(on_tree_p(t, x, y)){
      DBG("On tree: %ld %ld", x, y);
    }else{
      while(1){
	if (bt_new_particle_at(t, x, y)) break;
      }
      ret++;
    }
  }
  return ret; /* Number of particles that succeeded */
}

void bt_npart(struct brownian_tree* t, rp_gen f, ul n)
{
  ul x = 0, y = 0;
  for(ul i = 0; i < n; ){
    ul c = f(t->x, t->y); /* The function should encode two values in ul, TODO: long long? */
    y = c % t->y;
    x = c / t->y;
    if(on_tree_p(t, x, y)){
      DBG("On tree: %ld %ld", x, y);
    }else{
      while(1){
	/* Determinism of this code, is questionable.
	   Granted if num_partcles < width * height, the process end 
	   with probabilistic certainty, but there should be an optimum
	   level of density that should be warned when exceeded.
	*/
	if (bt_new_particle_at(t, x, y)) break;
      }
      i++; /* Increment for loop on successful particle. */
    }    
  }
}

struct point{
  ul x, y;
  struct point * next;
};

struct pqueue{
  struct point * front;
  struct point * last;
};

int bt_npart_threaded(struct brownian_tree* t, rp_gen f, ul n)
{
  int ret_num = 0;
  /* TODO: Figure out a crystal clear way to compose parallel computed
     Particles. */
  /* Thread manager should maintain a constant num of threads until the
     end condition is met.*/
  /* When each thread exits, if a new particle is generated queue it. */
  /* Queue control should Have the highest priority. */
  /* It should clear the queue and set the state as fastly as possible 
     to  minimize useless particles due to congestion. */
  return ret_num;
}
