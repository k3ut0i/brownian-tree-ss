
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
VPATH 		= $(SRC_DIR):$(TEST_DIR):$(BUILD_DIR)

SRCS = $(wildcard $(SRC_DIR)/*.c) $(wildcard $(TEST_DIR)/*.c)
OBJS = $(SRCS:.c=.o)
DEPS = $(OBJS:.o=.d)

sample-programs= center_seeded.bin	 		\
		 center_origin_circle_seeded.bin	\
		 isolated_points.bin			\
		 bt_scm_shell.bin			\
		 square_ends_seeded.bin



top: $(OBJS)

%.o : %.c
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -MMD -c $< -o $@

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

generate-samples: samples t/generate-images.pl
	perl t/generate-images.pl

clean-coverage:
	rm -f ./*/*.gcda ./*/*.gcno *.gcov gmon.out
clean:
	rm -f $(OBJS) $(DEPS) $(sample-programs) \
	src/*.x *.so *.o 
clean-all: clean clean-coverage
-include $(DEPS)
