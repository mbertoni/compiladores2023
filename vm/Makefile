all: macc

%.o: CFLAGS+= -Wall -Wextra -std=c11
macc: LDLIBS +=-lgc
	

clean:
	rm -f *.o macc

.PHONY: clean all
