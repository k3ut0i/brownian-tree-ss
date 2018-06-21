
CC = gcc
CFLAGS = -Wall -Wextra -I./src/
CFLAGS += `pkg-config --cflags guile-2.2`

ifdef DEBUG
CFLAGS += -ggdb -DDEBUG
endif

ifdef PROF
CFLAGS += -pg
endif

ifdef STATS
CFLAGS += -DSTATS
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
	$(CC) $(CFLAGS) -MMD -c $< -o $@

clean:
	rm -f $(OBJS) $(DEPS)

test-generate-simple-bt: simple_tree.o brownian_tree.o
	$(CC) $(CFLAGS) -o $@ $^
bt-scm-shell: brownian_tree.o bt_scm_shell.o
	$(CC) $(CFlAGS) -o $@ $^  `pkg-config --libs guile-2.2`

-include $(DEPS)
