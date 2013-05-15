/* Scanner for GP2. Created 10/5/2013 by Chris Bak */

%option noyywrap nodefault case-insensitive yylineno

%x COMMENT

%{

typedef enum {
   MAIN, IF, TRY, THEN, ELSE, SKIP, FAIL,                    /* Program text keywords */
   WHERE, AND, OR, NOT, EDGE, TRUE, FALSE, INDEG, OUTDEG,    /* Schema condition keywords */
   INT, STRING, ATOM, LIST,                                  /* Types keywords */
   LPAR, RPAR, LBRACE, RBRACE, 				     /* Left and right brackets */
   BAR, COMMA, ARROW,						     /* Delimiters */
   SEQ, ALAP,                                                /* Program operators */
   DOT, COLON,						     /* Label operators */
   ADD, SUB, MUL, DIV,  				     /* Arithmetic operators */
   EQ, GT, GTE, LT, LTE,				     /* Boolean operators */
   NUM, STR, ID,                                             /* Numbers, strings, identifiers */
   END 							     /* End of file */
} Token;

/* Potential issue: 'or' is a program keyword and a condition keyword. Not sure how to deal with this yet. */

/* Potential issue #2: Positions can be represented with decimal numbers. The compiler may be able to ignore this, depending on what the editor does. If not, then unclear whether to represent decimal numbers as a separate token from integers. Probably. */

/* Should (R) be its own token or not? I would think so. */

void printToken(Token t)
{
   if (t == MAIN)	printf("MAIN");
   if (t == IF)		printf("IF");
   if (t == TRY)	printf("TRY");
   if (t == THEN)	printf("THEN");
   if (t == ELSE)	printf("ELSE");
   if (t == SKIP)	printf("SKIP");
   if (t == FAIL)	printf("FAIL");
   if (t == WHERE)	printf("WHERE");
   if (t == AND)	printf("AND");
   if (t == OR)	        printf("OR");
   if (t == NOT)	printf("NOT");
   if (t == EDGE)	printf("EDGE");
   if (t == TRUE)	printf("TRUE");
   if (t == FALSE)	printf("FALSE");
   if (t == INDEG)	printf("INDEG");
   if (t == OUTDEG)	printf("OUTDEG");
   if (t == INT)	printf("INT");
   if (t == STRING)	printf("STRING");
   if (t == ATOM)	printf("ATOM");
   if (t == LIST)	printf("LIST");
   if (t == LPAR)	printf("LPAR");
   if (t == RPAR)	printf("RPAR");
   if (t == LBRACE)	printf("LBRACE");
   if (t == RBRACE)	printf("RBRACE");
   if (t == BAR)	printf("BAR");
   if (t == COMMA)	printf("COMMA");
   if (t == ARROW)	printf("ARROW");
   if (t == SEQ)	printf("SEQ");
   if (t == ALAP)	printf("ALAP");
   if (t == DOT)	printf("DOT");
   if (t == COLON)	printf("COLON");
   if (t == ADD)	printf("ADD");
   if (t == SUB)	printf("SUB");
   if (t == MUL)	printf("MUL");
   if (t == DIV)	printf("DIV");
   if (t == EQ)	        printf("EQ");
   if (t == GT)		printf("GT");
   if (t == GTE)	printf("GTE");
   if (t == LT)		printf("LT");
   if (t == LTE)	printf("LTE");
   if (t == NUM)	printf("NUM");
   if (t == STR)	printf("STR");
   if (t == ID)		printf("ID");
   if (t == END)	printf("END");
}

typedef union {
  int num;    /* A NUM token contains as number */
  char *str;  /* STRING and ID tokens contain a string */
} TokenValue;

TokenValue yylval;

char *curfilename;   /* name of current input file */

%}

%%

  

"/*"		    BEGIN(COMMENT);
<COMMENT>"*/"       BEGIN(INITIAL);
<COMMENT>([^*]|\n)+|.  /* ignore all characters except '*' */
<COMMENT><<EOF>>   { printf("%s:%d: Unterminated comment\n", curfilename, yylineno); return 0; }

[0-9]+              { yylval.num = atoi(yytext); return NUM; } 
main		    return MAIN;
if	            return IF;
try		    return TRY;
then 	  	    return THEN;
else		    return ELSE;
skip                return SKIP;
fail	 	    return FAIL;
where		    return WHERE;
and                 return AND;
or                  return OR;
not                 return NOT;
edge                return EDGE;
true                return TRUE;
false               return FALSE;
indeg 		    return INDEG;
outdeg		    return OUTDEG;
int		    return INT;
string	            return STRING;
atom		    return ATOM;
list		    return LIST;
"("		    return LPAR;
")"		    return RPAR;
"{"		    return LBRACE;
"}"		    return RBRACE;
"|"		    return BAR;
","                 return COMMA;
";"		    return SEQ;
"!"		    return ALAP;
"." 		    return DOT;
":"		    return COLON;
"+"		    return ADD;
"-"		    return SUB;
"*"		    return MUL;
"/"		    return DIV;
"="                 return EQ;
"=>"                return ARROW;
">="	            return GTE;
">" 		    return GT; 
"<="	            return LTE;
"<"		    return LT;
[_a-z][a-z0-9_-]*   { yylval.str = yytext; return ID; }
\"[a-z]*\"          { yylval.str = yytext; return STR; }
[ \t\n\r]+          /* ignore white space */
<<EOF>>             return END;      
.                   printf("%s:%d: Mystery character '%s'\n", curfilename, yylineno, yytext);

%%

int main(int argc, char** argv) {
  Token t;
  int i;
  if(argc < 2) {
    fprintf(stderr, "ERROR: filename required\n");
    return 1;
  }
  for(i=1; i<argc; i++) {

     if(!(yyin = fopen(argv[i], "r"))) {
       perror(argv[1]);
       return 1;
       }
  
  curfilename = argv[i];
  printf("Processing %s\n\n", curfilename);

     do {
       t = yylex();
       printToken(t);       
       if (t == NUM) printf("(%d)", yylval.num);
       if (t == STRING) printf("(%s)", yylval.str);
       if (t == ID) printf("(%s)", yylval.str);
       printf(" ");
     } while(t!=END);  
     
  printf("\n\n");
  } 

return 0;
} 


