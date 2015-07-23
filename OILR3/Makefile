CC=gcc -g -O2 -Wall -Werror


oilrrt :

% : %.c
	$(CC) $< -o $@

count :
	egrep -v '^[ \t]*(//.*)?$$' oilrrt.c | wc -l

clean :
	rm -f *.o $(TARGETS) a.out


