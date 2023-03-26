# https://makefiletutorial.com/#-wildcard-1
PROG ?= senseic
SENSEI_OBJS = main.o sensei.o
# TODO extract from autoconf
LD_FLAGS=-L/opt/homebrew/opt/openssl@3/lib
CFLAGS=-I/opt/homebrew/opt/openssl@3/include

all: $(PROG)
# Debug version.
#

$(PROG): $(SENSEI_OBJS)
	$(CC) -O0 -g3 -W -Wall -Werror $(CFLAGS) $(LD_FLAGS) -o $(PROG) $^ -lssl -lcrypto

test-runner: test-runner.o sensei.o
	${CC} $(CFLAGS) $(LD_FLAGS) -lcrypto -o $@ $^

test: test-runner
	./test-runner

clean:
	rm -rf $(PROG) *.o *.obj
