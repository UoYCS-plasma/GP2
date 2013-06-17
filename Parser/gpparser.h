/* /////////////////////////////////////////////////////////////////////////////////////// */

/*                                     gpparser.h
 *                                     Version 1.0
 * 
 * This is a header file for the GP2 parser. It contains an interface to the lexer and
 * some other stuff which I will identify later.
 *
 * Created on 28/5/13 by Chris Bak */


/* /////////////////////////////////////////////////////////////////////////////////////// */

/* interface to the lexer */
extern int yylineno;	
extern FILE* yyin;	
extern char *curfilename;	/* all from gplexer.lex */

/* Abstract syntax tree goes here too */

