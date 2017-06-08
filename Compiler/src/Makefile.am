BUILT_SOURCES = parser.h parser.c
AM_YFLAGS = -d

bin_PROGRAMS = gp2

gp2_SOURCES = ast.c ast.h error.c error.h genCondition.c genCondition.h \
              genLabel.c genLabel.h genProgram.c genProgram.h \
              lexer.lex parser.y main.c pretty.c pretty.h rule.c rule.h \
              searchplan.c searchplan.h seman.c seman.h symbol.c symbol.h \
              transform.c transform.h

gp2_CFLAGS = `pkg-config --cflags --libs glib-2.0`
gp2_LDFLAGS = -lglib-2.0 


