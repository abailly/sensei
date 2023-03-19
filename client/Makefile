PROG ?= senseic
SENSEI_OBJS = main.o sensei.o
# TODO extract from autoconf
LD_FLAGS="-L/opt/homebrew/opt/openssl@3/lib"
CPP_FLAGS="-I/opt/homebrew/opt/openssl@3/include"

all: $(PROG)
# Debug version.
#

.c.o:
	$(CC) -c -Wall -Werror $(CPP_FLAGS) $<

$(PROG): main.o sensei.o
	$(CC) -O0 -g3 -W -Wall -Werror $(CPP_FLAGS) $(LD_FLAGS) -o $(PROG) $(SENSEI_OBJS) -lssl -lcrypto

test-runner: test-runner.o main.o
	${CC} -o $@ $< ${LDFLAGS}

test: test-runner
	./test-runner

clean:
	rm -rf $(PROG) *.o *.obj
