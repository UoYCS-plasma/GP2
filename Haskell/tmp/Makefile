DEFS=-DDEBUG -DOILR_STANDALONE
CFLAGS=
CC=gcc -g -Wall -Werror $(DEFS)
LIBS=oilrrt.a
TARGETS=transitive_closure structural_sierpinski
INCS=oilrrt.h oilrinst.h

all : $(TARGETS)

% : %.o $(LIBS)
	$(CC) -o $@ $< $(LIBS)

%.o : %.c $(INCS)
	$(CC) -o $@ -c $<

%.c : %.gp2 %.host
	../gp2c $^


.SECONDARY : %.c

