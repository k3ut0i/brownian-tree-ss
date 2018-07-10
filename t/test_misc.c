#include "test_misc.h"

int populate_config(int argc, char* argv[])
{
  int ret = 0;
  int c;
  while(1){
    int option_index = 0;
    c = getopt_long(argc, argv, "w:h:r:o:n:s:", long_options, &option_index);
    if(c == -1)
      break;  
    switch(c){
    case 'w':
      config.width = get_ulong(optarg);
      ret += 1 << 0;
      break;
    case 'h':
      config.height = get_ulong(optarg);
      ret += 1 << 1;
      break;
    case 'r':
      config.rseed = get_ulong(optarg);
      ret += 1 << 2;
      break;
    case 'o':
      config.outfile = optarg;
      ret += 1 << 3;
      break;
    case 'n':
      config.npart = get_ulong(optarg);
      ret += 1 << 4;
      break;
    case 's':
      config.nseeds = get_ulong(optarg);
      ret += 1 << 5;
      break;
    case '?':
      break;
    default:
      break;
    }
  }
  return ret;
}
