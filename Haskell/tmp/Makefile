DEFS=-DDEBUG -DOILR_STANDALONE
CFLAGS=
CC=gcc -g -Wall -Werror $(DEFS)
OBJS=transitive_closure.o
SRCS=transitive_closure.c
LIBS=oilrrt.a
TARGETS=transitive_closure
INCS=oilrrt.h oilrinst.h

$(TARGETS) : $(OBJS) $(LIBS)

$(OBJS) : $(SRCS) $(INCS)

% : %.o
	$(CC) -o $@ $< $(LIBS)

%.o : %.c
	$(CC) -o $@ -c $<

%.c : %.gp2 %.host
	../gp2c $^



