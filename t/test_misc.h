#ifndef _TEST_MISC_H
#define _TEST_MISC_H
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>

#define get_ulong(str) strtoul(str, NULL, 0)

const struct option long_options[] =
  {
   {"width", required_argument, 0, 'w'},
   {"height", required_argument, 0, 'h'},
   {"rseed", required_argument, 0, 'r'},
   {"outfile", required_argument, 0, 'o'},
   {"npart", required_argument, 0, 'n'},
   {"nseeds", required_argument, 0, 's'},
   {0, 0, 0, 0}
  };

/* Initial configuration for test programs. */
struct test_config {
  unsigned long  width, /* 1 << 0 */
    height,             /* 1 << 1 */
    rseed,              /* 1 << 2 */
    npart,              /* 1 << 3 */
    nseeds;             /* 1 << 4 */
  char* outfile;        /* 1 << 5 */
} config;


int populate_config(int argc, char* argv[]);
int validate_config();
#endif /* _TEST_MISC_H */
