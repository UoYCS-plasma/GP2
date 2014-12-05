DEFINES=-DOILR_STANDALONE -DDEBUG
CC=gcc -g -Wall -Werror $(DEFINES)

TARGETS=oilrrt.o

all : $(TARGETS)

%.o : %.c oilrrt.h oilrinst.h invariants.c
	$(CC) -c $<

clean :
	rm -f *.o


