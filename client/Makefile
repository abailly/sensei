# https://makefiletutorial.com/#-wildcard-1
PROG ?= senseic
SENSEI_OBJS = main.o sensei.o
SENSEI_TEST_OBJS = tests/sensei-test.o
# TODO extract from autoconf
CURRENT_DIR = $(shell pwd)
LD_FLAGS=-L/opt/homebrew/opt/openssl@3/lib
CFLAGS=-I/opt/homebrew/opt/openssl@3/include -I$(CURRENT_DIR)

all: $(PROG)
# Debug version.
#

$(PROG): $(SENSEI_OBJS)
	$(CC) -O0 -g3 -W -Wall -Werror $(CFLAGS) $(LD_FLAGS) -o $(PROG) $^ -lssl -lcrypto

test-runner: sensei.o $(SENSEI_TEST_OBJS)
	${CC} $(CFLAGS) $(LD_FLAGS) -lcrypto -o $@ $^

test: test-runner
	./test-runner

clean:
	rm -rf $(PROG) *.o *.obj
