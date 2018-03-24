
CC = gcc
CFLAGS = -Wall -Wextra -ggdb -I./src/

.PHONY: clean top all-tests

BUILD_DIR 	= _build
SRC_DIR 	= src
TEST_DIR 	= t
VPATH 		= $(SRC_DIR):$(TEST_DIR):$(BUILD_DIR)

SRCS = $(wildcard $(SRC_DIR)/*.c) $(wildcard $(TEST_DIR)/*.c)
OBJS = $(SRCS:.c=.o)
DEPS = $(OBJS:.o=.d)



top: $(OBJS)

all-tests: test-generate-simple-bt

$(BUILD_DIR)/%.o : $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -MMD -c $< -o $@

clean:
	rm -f $(OBJS) $(DEPS)

test-generate-simple-bt: simple_tree.o brownian_tree.o
	$(CC) $(CFLAGS) -o $@ $^

-include $(DEPS)
