CC = gcc
CFLAGS = -Wall -Wextra -ggdb

.PHONY: clean top

SRCS = $(wildcard *.c)

top:

%.o : %.c
	@$(CC) $(CFLAGS) -c $<
	@echo "CC" $@

clean:
	rm -f $(OBJECTS)
