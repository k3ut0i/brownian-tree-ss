#include "test_misc.h"
#include <assert.h>

const struct option long_options[] =
  {
   {"width", required_argument, 0, 'w'},
   {"height", required_argument, 0, 'h'},
   {"rseed", required_argument, 0, 'r'},
   {"outfile", required_argument, 0, 'o'},
   {"npart", required_argument, 0, 'n'},
   {"nseeds", required_argument, 0, 's'},
   {"test_type", required_argument, 0, 't'},
   {0, 0, 0, 0}
  };

const struct option_desc long_options_desc[] =
  {
   {'w', "width of the image, unsigned long"},
   {'h', "height of the image, unsigned long"},
   {'r', "seed for random number generation, unsigned long"},
   {'o', "output file to dump image or info, file name"},
   {'n', "number of particles in the tree, unsigned long"},
   {'s', "number of initial seeds; for some algos, unsigned long"},
   {'t', "image type, Options are 'center_seeded' ."}
  };


int populate_config(int argc, char* argv[])
{
  int ret = 0;
  int c;
  while(1){
    int option_index = 0;
    c = getopt_long(argc, argv, "w:h:r:o:n:s:t:", long_options, &option_index);
    if(c == -1)
      break;  
    switch(c){
    case 'w':
      config.width = get_ulong(optarg);
      ret += WIDTH;
      break;
    case 'h':
      config.height = get_ulong(optarg);
      ret += HEIGHT;
      break;
    case 'r':
      config.rseed = get_ulong(optarg);
      ret += RSEED;
      break;
    case 'o':
      config.outfile = optarg;
      ret += OUTFILE;
      break;
    case 'n':
      config.npart = get_ulong(optarg);
      ret += NPART;
      break;
    case 's':
      config.nseeds = get_ulong(optarg);
      ret += NSEEDS;
      break;
    case 't':
      config.test_type = optarg;
      ret += TEST_TYPE;
    case '?':
      break;
    default:
      break;
    }
  }
  return ret;
}

void print_banner(const char* s){
  fprintf(stderr, "Options for program %s\n", s);
  int n = sizeof(long_options)/sizeof(struct option);
  for(int i = 0; i < n - 1; i++){
    assert(long_options[i].val == long_options_desc[i].val);
    fprintf(stderr, "\t-%c, --%-10s %s\n",
	    long_options[i].val, long_options[i].name,
	    long_options_desc[i].desc);
  }
}
