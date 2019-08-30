BUILT_SOURCES = lexer.c parser.h parser.c
AM_YFLAGS = -d -Wno-yacc
AM_CFLAGS = -l:Judy

lib_LIBRARIES = libgp2.a

libgp2_a_SOURCES = debug.c graph.c graphStacks.c label.c morphism.c \
                   lexer.l parser.y arrays.c
include_HEADERS = common.h debug.h graph.h graphStacks.h label.h \
                  morphism.h parser.h  

CLEANFILES = parser.c parser.h 
