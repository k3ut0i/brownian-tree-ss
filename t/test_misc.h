#ifndef _TEST_MISC_H
#define _TEST_MISC_H
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>

#define get_ulong(str) strtoul(str, NULL, 0)

struct option_desc {
  int val;
  char* desc; 
};
extern const struct option long_options[];
extern const struct option_desc long_options_desc[];

/* Initial configuration for test programs. */
struct test_config {
  unsigned long  width, /* 1 << 0 */
    height,             /* 1 << 1 */
    rseed,              /* 1 << 2 */
    npart,              /* 1 << 3 */
    nseeds;             /* 1 << 4 */
  char* outfile;        /* 1 << 5 */
  char* test_type;      /* 1 << 6 */
} config;

enum test_config_key
  {
   WIDTH  = 1 << 0,
   HEIGHT = 1 << 1,
   RSEED  = 1 << 2,
   NPART  = 1 << 3,
   NSEEDS = 1 << 4,
   OUTFILE= 1 << 5,
   TEST_TYPE = 1 << 6
  }config_key;

int populate_config(int argc, char* argv[]);
void print_banner(const char* name);
#endif /* _TEST_MISC_H */
