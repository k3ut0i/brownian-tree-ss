
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
CFLAGS += -pg
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



top: $(OBJS)

all-tests: test-generate-simple-bt

%.o : %.c
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -MMD -c $< -o $@

libbrownian_tree.so: brownian_tree.o
	$(CC) $(CFLAGS) $(LIBS) -shared -o $@ $^

center_origin_circle_seeded: center_origin_circle_seeded.o brownian_tree.o
	$(CC) $(CFLAGS) $(LIBS) -o $@ $^

test-generate-simple-bt: simple_tree.o brownian_tree.o
	$(CC) $(CFLAGS) $(LIBS) -o $@ $^

bt-scm-shell: brownian_tree.o bt_scm_shell.o
	$(CC) $(CFlAGS) $(LIBS) -o $@ $^

snarfcppopts = $(DEFS) $(INCLUDES) $(CFLAGS)
.SUFFIXES: .x
%.x : %.c
	guile-snarf -o $@ $< $(snarfcppopts)


bt-guile-bindings.o : src/bt-guile-bindings.x
bt-guile-bindings.so: bt-guile-bindings.o brownian_tree.o
	$(CC) $(CFLAGS) $(LIBS) -shared -o $@ $^

test_all: test-generate-simple-bt
	prove
clean:
	rm -f $(OBJS) $(DEPS) src/*.x *.so *.o test-generate-simple-bt bt-scm-shell

-include $(DEPS)
