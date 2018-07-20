
CC = gcc
DEFS =
CFLAGS = -Wall -Wextra -fPIC
INCLUDES = -I./src/ $(shell pkg-config --cflags guile-2.2)
LIBS = $(shell pkg-config --libs guile-2.2) -lm

ifdef DEBUG
CFLAGS += -ggdb
DEFS   += -DDEBUG
endif

ifdef PROF
CFLAGS += -pg -fprofile-arcs -ftest-coverage
LIBS += -lgcov
endif

ifdef STATS
DEFS += -DSTATS
endif

.PHONY: clean top all-tests

SRC_DIR 	= src
TEST_DIR 	= t
EXAMPLE_DIR	= examples
VPATH 		= $(SRC_DIR):$(TEST_DIR):$(EXAMPLE_DIR)

SRCS = $(wildcard $(SRC_DIR)/*.c) $(wildcard $(TEST_DIR)/*.c) \
	$(wildcard $(EXAMPLE_DIR)/*.c)

OBJS = $(SRCS:.c=.o)
DEPS = $(OBJS:.o=.d)

sample-programs= center_seeded.bin	 		\
		 center_origin_circle_seeded.bin	\
		 isolated_points.bin			\
		 bt_scm_shell.bin			\
		 square_ends_seeded.bin
test-programs = dump_tests.test.bin
example-programs = scripted_seeds.bin

top: $(OBJS)

%.o : %.c
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -MMD -c $< -o $@

%.test.bin: %.o brownian_tree.o test_misc.o
	$(CC) $(CFLAGS) $(LIBS) -o $@ $^

%.bin : %.o brownian_tree.o
	$(CC) $(CFLAGS) $(LIBS) -o $@ $^

libbrownian_tree.so: brownian_tree.o
	$(CC) $(CFLAGS) $(LIBS) -shared -o $@ $^

snarfcppopts = $(DEFS) $(INCLUDES) $(CFLAGS)
%.x : %.c
	guile-snarf -o $@ $< $(snarfcppopts)

bt-guile-bindings.o : src/bt-guile-bindings.x
bt-guile-bindings.so: bt-guile-bindings.o brownian_tree.o
	$(CC) $(CFLAGS) $(LIBS) -shared -o $@ $^

samples: $(sample-programs)
examples: $(example-programs)

generate-samples: samples t/generate-images.pl
	perl t/generate-images.pl
test:	dump_tests.test.bin
	./dump_tests.test.bin -w 100 -h 100 -r 1 -o "dump_info" -n 1000 -t "center_seeded"
clean-samples:
	rm -f $(sample-programs)
clean-examples:
	rm -f $(example-programs)
clean-coverage:
	rm -f ./*/*.gcda ./*/*.gcno *.gcov gmon.out
clean:
	rm -f $(OBJS) $(DEPS) src/*.x *.so *.o 
clean-all: clean clean-coverage clean-samples clean-examples
-include $(DEPS)
