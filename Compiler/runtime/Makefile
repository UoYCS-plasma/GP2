# Runtime objects.
ROBJECTS := $(patsubst %.c, %.o, $(wildcard *.c)) $(patsubst %.c, %.o, $(wildcard host/*.c)) 
# Library objects.
LOBJECTS = ../debug.o ../error.o ../label.o ../graph.o ../graphStacks.o ../rule.o 
OBJECTS = $(ROBJECTS) $(LOBJECTS)

CC = gcc 
CFLAGS = -g -O0 -Wall -Wextra 
#CFLAGS = -O2 -fomit-frame-pointer -Wall -Wextra 
LFLAGS = -lglib-2.0

default:	$(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o GP2-run 		

%.o:		%.c
		$(CC) -c $(CFLAGS) -o $@ $<

# Delete all files except Makefile, labelMatch*, and morphism*. 
clean:		
		ls | grep -v 'host\|Makefile\|labelMatch\|morphism' | xargs rm
		rm host/*
		rm match.o
