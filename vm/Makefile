all: macc

%.o: CFLAGS+= -Wall -pedantic
macc: macc.o # LDLIBS +=-lgc
	gcc $< -lgc

clean:
	rm -f *.o macc

.PHONY: clean all
