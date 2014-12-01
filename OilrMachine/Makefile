CC=gcc
# -Wall -Wextra

TARGETS=oilrrt.o

oilrrt_s.o : oilrrt.o
	cp $< $@



%.o : %.c
	$(CC) -g -c $(CFLAGS) $<

clean :
	rm -f *.o


