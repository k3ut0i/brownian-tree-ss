#include <libguile.h>

#define getul(str) strtoul(str, NULL, 0)

#define handle_args(argc, argv)			\
  if(argc != 6){				\
    fprintf(stderr, "Usage: %s <size> <rseed> <npart> <outfile> <scriptfile>",\
	    argv[0]);							\
    exit(EXIT_FAILURE);							\
  }									\
  unsigned long size = getul(argv[1]);					\
  unsigned long rseed = getul(argv[2]);					\
  unsigned long npart = getul(argv[3]);					\
  const char* outfile = argv[4];					\
  const char* scriptfile = argv[5];    


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

