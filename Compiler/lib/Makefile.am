BUILT_SOURCES = lexer.c parser.h parser.c
AM_YFLAGS = -d -Wno-yacc
AM_CFLAGS = -lJudy

lib_LIBRARIES = libgp2.a

libgp2_a_SOURCES = arrays.c graph.c graphStacks.c label.c morphism.c lexer.l parser.y arrays.c
include_HEADERS = common.h graph.h graphStacks.h label.h morphism.h parser.h  

CLEANFILES = parser.c parser.h 
