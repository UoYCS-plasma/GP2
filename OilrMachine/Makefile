DEFS=-DOILR_STANDALONE -DDEBUG
CC=gcc -g -Wall -Werror $(DEFS)

TARGETS=oilrrt.a

$(TARGETS) : $(OBJS)

#../Compiler/oilrrc.o :
#	$(MAKE) -C 

oilrrt.a : oilrrt.o
	ar rcs $@ $^

%.o : %.c oilrrt.h oilrinst.h invariants.c
	$(CC) -c $<

clean :
	rm -f *.o


