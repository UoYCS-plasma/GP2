CC=gcc -g -Wall -Werror $(DEFS)

TARGETS=oilrrt.a
OBJS=graph.o oilrrt.o
HEADERS=graph.h

# $(TARGETS) : $(OBJS)


oilrrt.a : $(OBJS)
	ar rcs $@ $^

%.o : %.c $(HEADERS)
	$(CC) -c $<

clean :
	rm -f *.o $(TARGETS) a.out


