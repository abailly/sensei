PROG ?= client
LD_FLAGS="-L."
CPP_FLAGS="-I/opt/homebrew/opt/openssl@3/include"

all: $(PROG)
# Debug version.
#
$(PROG): main.c
	$(CC) -O0 -g3 -W -Wall -Werror $(CPP_FLAGS) $(LD_FLAGS) -o $(PROG) main.c -lssl -lcrypto

clean:
	rm -rf $(PROG) *.o *.obj
