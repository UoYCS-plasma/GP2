BUILT_SOURCES = lexer.c parser.h parser.c
AM_YFLAGS = -d

bin_PROGRAMS = gp2

gp2_CFLAGS = $(GLIB_CFLAGS) 
gp2_SOURCES = ast.c ast.h error.c error.h genCondition.c genCondition.h \
              genLabel.c genLabel.h genProgram.c genProgram.h genRule.c \
              genRule.h lexer.l parser.y main.c pretty.c pretty.h rule.c \
              rule.h searchplan.c searchplan.h seman.c seman.h symbol.c \
              symbol.h transform.c transform.h
gp2_LDADD = $(GLIB_LIBS) 

TESTS = test.sh # Run when 'make check' is called.

# Run when 'make installcheck' is called.
installcheck-local:	installtest.sh 
			./installtest.sh
