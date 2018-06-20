
CC = gcc
CFLAGS = -Wall -Wextra -I./src/

ifdef DEBUG
CFLAGS += -ggdb
endif

ifdef PROF
CFLAGS += -pg
endif

ifdef STATS
export STATS = 1
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

-include $(DEPS)
