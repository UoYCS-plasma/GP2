CC=gcc -Wall -Wextra `pkg-config --cflags --libs glib-2.0`

OBJECTS=oilrruntime.o Compiler/graph.o Compiler/graph.o Compiler/stack.o

runtime.a : $(OBJECTS)
	ar rcs $@ $(OBJECTS)
	ranlib $@

%.o : %.c
	$(CC) -g -c $<

clean :
	rm -f *.o runtime.a


