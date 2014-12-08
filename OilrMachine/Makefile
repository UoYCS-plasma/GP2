DEFINES=-DOILR_STANDALONE -DDEBUG
CC=gcc -g -Wall -Werror $(DEFINES)

TARGETS=oilrrt.a

$(TARGETS) : $(OBJS)

oilrrt.a : oilrrt.o
	ar rcs $@ $^

%.o : %.c oilrrt.h oilrinst.h invariants.c
	$(CC) -c $<

clean :
	rm -f *.o


