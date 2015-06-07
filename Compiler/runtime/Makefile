# Runtime objects.
ROBJECTS := $(patsubst %.c, %.o, $(wildcard *.c)) $(patsubst %.c, %.o, $(wildcard host/*.c)) 
# Library objects.
LOBJECTS = ../debug.o ../error.o ../label.o ../graph.o ../graphStacks.o ../rule.o 
OBJECTS = $(ROBJECTS) $(LOBJECTS)

CC = gcc 
CFLAGS = -I .. -g -pg -O0 -Wall -Wextra  
#CFLAGS = -I .. -O2 -fomit-frame-pointer -Wall -Wextra 
LFLAGS = -pg -lglib-2.0

default:	$(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o GP2-run 		

%.o:		%.c
		$(CC) -c $(CFLAGS) -o $@ $<

# Delete all files except Makefile, morphism*, and the parsing files. 
clean:		
		ls | grep -v 'host\|Makefile\|morphism' | xargs rm
		if [ -d "host" ]; then rm host/*; rmdir host; fi
		if [ -e "morphism.o" ]; then rm morphism.o; fi
		if [ -e "hostParser.o" ]; then rm hostParser.o; fi
		if [ -e "hostLexer.o" ]; then rm hostLexer.o; fi
