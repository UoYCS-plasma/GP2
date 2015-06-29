# Runtime objects.
ROBJECTS := $(patsubst %.c, %.o, $(wildcard *.c)) $(patsubst %.c, %.o, $(wildcard host/*.c)) 
# Library objects.
LOBJECTS = ../lib/debug.o ../lib/graph.o ../lib/graphStacks.o ../lib/hostLexer.o \
           ../lib/hostParser.o ../lib/label.o ../lib/morphism.o
           
OBJECTS = $(ROBJECTS) $(LOBJECTS)

CC = gcc 
#CFLAGS = -I../lib -I../inc -g -O0 -Wall -Wextra  
CFLAGS = -I../lib -I../inc -O2 -fomit-frame-pointer -Wall -Wextra 

default:	$(OBJECTS)
		$(CC) $(OBJECTS) -o GP2-run 		

%.o:		%.c
		$(CC) -c $(CFLAGS) -o $@ $<

# Delete all files except Makefile (this file). 
clean:		
		ls | grep -v 'Makefile' | xargs rm
