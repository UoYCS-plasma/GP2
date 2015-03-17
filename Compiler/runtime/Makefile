ROBJECTS := $(patsubst %.c, %.o, $(wildcard *.c))
LOBJECTS = ../debug.o ../error.o ../label.o ../graph.o ../rule.o ../stack.o 
OBJECTS = $(ROBJECTS) $(LOBJECTS)

CC = gcc 
CFLAGS = -g -Wall -Wextra 
#CFLAGS = -O2 -fomit-frame-pointer -Wall -Wextra 
LFLAGS = -lglib-2.0

default:	$(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o GP2-run 		

%.o:		%.c
		$(CC) -c $(CFLAGS) -o $@ $<

# Delete all files except Makefile, match.c and match.h. 
clean:		
		ls | grep -v 'Makefile\|match' | xargs rm
		rm match.o
