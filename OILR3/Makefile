CC=gcc -g -O2 -Wall -Werror

TARGETS=oilrrt.a
OBJS=graph.o
HEADERS=graph.h

# $(TARGETS) : $(OBJS)


oilrrt.a : oilrrt.o $(OBJS)
	ar rcs $@ $^

oilrrt : oilrrt.o $(OBJS)
	$(CC) -o $@ $^

testGraph : testGraph.o $(OBJS)
	$(CC) -o $@ $^

%.o : %.c $(HEADERS)
	$(CC) -c $<

count :
	egrep -v '^[ \t]*(//.*)?$$' oilrrt.c | wc -l

clean :
	rm -f *.o $(TARGETS) a.out


