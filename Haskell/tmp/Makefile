DEFINES=-DDEBUG -DOILR_STANDALONE
CFLAGS=
CC=gcc -g -Wall -Werror $(DEFINES)
OBJS=oilrrt.o
TARGETS=transitive_closure


all : $(TARGETS)

% : %.o oilrrt.o
	$(CC) -o $@ $+

%.o : %.c oilrrt.h oilrinst.h
	$(CC) -o $@ -c $<

%.c : %.gp2 %.host
	../gp2c $^



