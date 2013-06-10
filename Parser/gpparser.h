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
extern int yylineno;	/* from lexer */
int yydebug = 1;	/* generates y.output for debugging */
void yyerror(char *s, ...);	/* takes multiple arguments, not sure if necessary yet */

/* Abstract syntax tree goes here too */

