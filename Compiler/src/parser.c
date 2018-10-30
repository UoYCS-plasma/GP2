/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 56 "parser.y" /* yacc.c:339  */

#include "common.h"

int yylex(void);

void yyerror(const char *error_message);
void report_warning(const char *error_message);

/* Flags used in the AST construction. */
bool is_root = false;
bool is_bidir = false;

/* Pointers to data structures constructed by the parser. */
struct List *gp_program = NULL; 
int host_nodes = 0, host_edges = 0;

bool syntax_error = false;

#line 85 "parser.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "parser.h".  */
#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 42 "parser.y" /* yacc.c:355  */

#include "ast.h"

#line 119 "parser.c" /* yacc.c:355  */

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    MAIN = 258,
    IF = 259,
    TRY = 260,
    THEN = 261,
    ELSE = 262,
    SKIP = 263,
    FAIL = 264,
    BREAK = 265,
    WHERE = 266,
    EDGETEST = 267,
    INDEG = 268,
    OUTDEG = 269,
    _LENGTH = 270,
    INT = 271,
    CHARACTER = 272,
    STRING = 273,
    ATOM = 274,
    LIST = 275,
    INTERFACE = 276,
    _EMPTY = 277,
    INJECTIVE = 278,
    MARK = 279,
    ANY_MARK = 280,
    ARROW = 281,
    NEQ = 282,
    GTEQ = 283,
    LTEQ = 284,
    NUM = 285,
    DNUM = 286,
    STR = 287,
    PROCID = 288,
    ID = 289,
    ROOT = 290,
    BIDIRECTIONAL = 291,
    GP_PROGRAM = 292,
    GP_GRAPH = 293,
    GP_RULE = 294,
    OR = 295,
    AND = 296,
    NOT = 297,
    UMINUS = 298
  };
#endif
/* Tokens.  */
#define MAIN 258
#define IF 259
#define TRY 260
#define THEN 261
#define ELSE 262
#define SKIP 263
#define FAIL 264
#define BREAK 265
#define WHERE 266
#define EDGETEST 267
#define INDEG 268
#define OUTDEG 269
#define _LENGTH 270
#define INT 271
#define CHARACTER 272
#define STRING 273
#define ATOM 274
#define LIST 275
#define INTERFACE 276
#define _EMPTY 277
#define INJECTIVE 278
#define MARK 279
#define ANY_MARK 280
#define ARROW 281
#define NEQ 282
#define GTEQ 283
#define LTEQ 284
#define NUM 285
#define DNUM 286
#define STR 287
#define PROCID 288
#define ID 289
#define ROOT 290
#define BIDIRECTIONAL 291
#define GP_PROGRAM 292
#define GP_GRAPH 293
#define GP_RULE 294
#define OR 295
#define AND 296
#define NOT 297
#define UMINUS 298

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 77 "parser.y" /* yacc.c:355  */
  
  int num;   /* value of NUM token. */
  double dnum; /* value of DNUM token. */
  char *str; /* value of STRING and CHAR tokens. */
  char *id;  /* value of PROCID and ID tokens. */
  int mark;  /* enum MarkTypes, value of MARK token. */
#line 110 "parser.y" /* yacc.c:355  */
  
  struct List *list; 
  struct GPDeclaration *decl;
  struct GPCommand *command;
  struct GPProcedure *proc;
  struct GPRule *rule;
  struct GPGraph *graph;
  struct GPNode *node;
  struct GPEdge *edge;
  struct GPPos *pos;
  struct GPCondition *cond_exp;
  struct GPLabel *label;  
  struct GPAtom *atom_exp;

  int list_type; /* enum ListType */
  int check_type; /* enum CondExpType */

#line 242 "parser.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;
int yyparse (void);
/* "%code provides" blocks.  */
#line 47 "parser.y" /* yacc.c:355  */

extern List *gp_program; 
extern int yylineno;
extern string yytext;
extern FILE *yyin;
extern bool syntax_error;

#line 277 "parser.c" /* yacc.c:355  */

#endif /* !YY_YY_PARSER_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 283 "parser.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  20
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   435

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  64
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  48
/* YYNRULES -- Number of rules.  */
#define YYNRULES  159
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  347

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   298

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    57,     2,    63,     2,     2,     2,     2,
      55,    56,    45,    43,    54,    44,    48,    46,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    49,    53,
      61,    50,    62,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    51,     2,    52,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    58,    60,    59,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    47
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   220,   220,   221,   222,   225,   227,   230,   231,   232,
     234,   236,   240,   245,   252,   253,   255,   258,   259,   261,
     266,   267,   268,   270,   273,   277,   279,   281,   283,   287,
     288,   292,   293,   294,   295,   296,   297,   299,   300,   301,
     303,   306,   308,   310,   313,   318,   326,   329,   333,   342,
     349,   354,   360,   362,   365,   366,   369,   372,   374,   377,
     378,   379,   380,   381,   385,   386,   387,   388,   389,   390,
     393,   394,   396,   399,   403,   404,   406,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   422,   423,   425,   426,
     430,   431,   433,   435,   438,   439,   440,   441,   442,   443,
     444,   445,   446,   447,   449,   450,   451,   452,   454,   455,
     458,   459,   461,   464,   465,   466,   467,   469,   470,   471,
     472,   474,   476,   477,   478,   479,   480,   481,   482,   483,
     486,   487,   488,   489,   497,   498,   506,   510,   511,   512,
     513,   515,   517,   520,   521,   523,   525,   528,   529,   531,
     534,   536,   537,   539,   540,   541,   542,   544,   545,   546
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "MAIN", "IF", "TRY", "THEN", "ELSE",
  "SKIP", "FAIL", "BREAK", "WHERE", "EDGETEST", "INDEG", "OUTDEG",
  "_LENGTH", "INT", "CHARACTER", "STRING", "ATOM", "LIST", "INTERFACE",
  "_EMPTY", "INJECTIVE", "MARK", "ANY_MARK", "ARROW", "NEQ", "GTEQ",
  "LTEQ", "NUM", "DNUM", "STR", "PROCID", "ID", "ROOT", "BIDIRECTIONAL",
  "GP_PROGRAM", "GP_GRAPH", "GP_RULE", "OR", "AND", "NOT", "'+'", "'-'",
  "'*'", "'/'", "UMINUS", "'.'", "':'", "'='", "'['", "']'", "';'", "','",
  "'('", "')'", "'!'", "'{'", "'}'", "'|'", "'<'", "'>'", "'#'", "$accept",
  "Initialise", "Program", "Declaration", "MainDecl", "ProcDecl",
  "LocalDecls", "ComSeq", "Command", "Block", "SimpleCommand",
  "RuleSetCall", "IDList", "RuleDecl", "VarDecls", "VarList", "Inter",
  "NodeIDList", "Type", "Graph", "NodeList", "Node", "EdgeList", "Edge",
  "Position", "RootNode", "Bidirection", "CondDecl", "Condition",
  "Subtype", "LabelArg", "Label", "List", "AtomExp", "ProcID", "RuleID",
  "NodeID", "EdgeID", "Variable", "HostGraph", "HostNodeList", "HostNode",
  "HostEdgeList", "HostEdge", "HostID", "HostLabel", "HostList", "HostExp", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,    43,    45,    42,    47,   298,    46,    58,
      61,    91,    93,    59,    44,    40,    41,    33,   123,   125,
     124,    60,    62,    35
};
# endif

#define YYPACT_NINF -243

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-243)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     315,    15,    -5,   145,    61,    40,  -243,  -243,    15,  -243,
    -243,  -243,  -243,   -21,    51,   196,  -243,  -243,    42,    57,
    -243,   184,  -243,   116,   108,   105,    -2,   128,   135,    83,
     144,    86,  -243,   268,   189,   189,  -243,  -243,  -243,   184,
     145,   302,  -243,   155,   111,  -243,  -243,  -243,  -243,   302,
    -243,   276,   121,  -243,  -243,   164,   278,  -243,   192,  -243,
     165,   171,   207,   136,   281,  -243,  -243,  -243,   351,   353,
      27,   176,  -243,  -243,   184,   184,   189,  -243,    71,   108,
     164,   293,   108,    85,   203,   239,     3,   164,  -243,   205,
     142,   170,   213,   191,   186,  -243,   128,   283,  -243,   189,
     189,   189,   189,   212,   245,   245,  -243,  -243,  -243,  -243,
     184,  -243,  -243,   217,   257,  -243,  -243,  -243,  -243,  -243,
    -243,  -243,   184,   100,   237,   230,  -243,   305,   164,   164,
     319,    35,   238,   308,   341,   310,   311,   344,   190,  -243,
     285,   322,  -243,  -243,   342,  -243,   368,  -243,  -243,  -243,
    -243,   302,   293,   164,   302,  -243,  -243,   192,  -243,   287,
    -243,   231,    32,   352,   164,  -243,  -243,  -243,   347,   246,
     -13,  -243,  -243,  -243,   317,  -243,  -243,   318,   320,   323,
     354,  -243,   289,   128,   189,   189,  -243,    32,   327,  -243,
     118,   291,  -243,   331,   266,   328,   336,   377,   164,    32,
    -243,  -243,   333,    78,   366,  -243,  -243,  -243,  -243,   329,
    -243,   338,  -243,  -243,   377,   125,  -243,  -243,   357,  -243,
    -243,  -243,   295,  -243,   337,   194,  -243,    32,   377,  -243,
    -243,  -243,  -243,  -243,   128,  -243,   343,   345,   346,  -243,
    -243,  -243,   240,   240,   267,   -11,   274,  -243,  -243,   340,
    -243,   296,    -3,   348,  -243,  -243,  -243,  -243,   194,   194,
     321,   349,    -7,   253,   377,  -243,   355,   100,   100,   108,
     358,   260,  -243,   356,   218,   339,   240,   240,   240,   240,
     240,   100,  -243,  -243,   233,  -243,   100,  -243,    95,   232,
     194,   194,   108,   125,   125,   240,   240,   240,   240,  -243,
      35,   359,   360,   361,  -243,  -243,  -243,   274,  -243,  -243,
     157,   157,   358,   358,  -243,   364,   100,  -243,   365,  -243,
     367,  -243,   369,   350,   350,   274,   274,   274,   274,   370,
    -243,  -243,  -243,   100,  -243,   100,  -243,  -243,   373,   374,
     125,   125,   375,   376,  -243,  -243,  -243
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     0,     0,   130,   131,     2,     5,
       7,     8,     9,     0,     0,     0,     3,     4,     0,     0,
       1,     0,     6,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   143,     0,     0,     0,    34,    35,    36,     0,
       0,    10,    17,    20,    32,    37,    39,    38,    14,    11,
     136,     0,     0,    52,    14,     0,     0,   150,    86,   137,
       0,     0,     0,     0,     0,   144,    31,    41,     0,    25,
       0,     0,    44,    42,     0,     0,     0,    33,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    87,     0,
       0,     0,     0,     0,     0,   139,     0,     0,   147,     0,
       0,     0,     0,    29,     0,     0,    40,    18,    19,    21,
       0,    16,    15,     0,     0,    59,    60,    61,    62,    63,
      50,    53,     0,     0,     0,     0,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   138,
       0,     0,   141,   148,    22,    24,    26,    27,    30,    45,
      43,    12,     0,     0,    13,   133,   132,    86,    64,     0,
      71,     0,     0,     0,     0,   155,   157,   159,     0,     0,
     151,   153,    78,    84,     0,    82,    77,     0,     0,     0,
       0,   140,     0,     0,     0,     0,    51,     0,     0,    66,
       0,     0,    74,     0,     0,     0,     0,    90,     0,     0,
     158,   145,     0,     0,     0,    79,    83,    80,    85,     0,
     142,     0,    23,    28,    90,     0,   135,   134,    88,    68,
      75,    65,     0,    56,     0,     0,    47,     0,    90,   146,
     156,   154,   152,    81,     0,    48,     0,     0,     0,   115,
     118,   119,     0,     0,     0,   110,   113,   117,    89,     0,
      67,     0,     0,     0,   104,   105,   106,   107,     0,     0,
      91,     0,     0,   113,    90,    46,     0,     0,     0,     0,
     123,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    54,     0,    57,     0,   100,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
       0,     0,     0,     0,   124,    73,   116,   114,   111,   112,
     125,   126,   127,   128,   129,     0,     0,    55,     0,   103,
     101,   102,     0,    95,    94,    97,    99,    98,    96,     0,
     120,   121,   122,     0,    58,     0,    92,   149,     0,   108,
       0,     0,     0,     0,   109,    93,    76
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -243,  -243,  -243,   388,  -243,   247,   379,   -15,   292,   -31,
    -243,  -243,  -243,     4,   371,    23,  -152,  -243,   250,   -65,
     244,  -116,   185,  -174,   -83,   254,  -243,  -198,  -218,  -243,
    -243,    28,  -209,  -114,    20,    11,  -242,  -243,   -24,  -243,
     372,   -18,   270,   -86,   -91,   113,  -243,   211
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,     8,     9,    10,    11,    78,    41,    42,    43,
      44,    45,    71,    12,    51,    52,   197,   284,   120,    85,
     125,   126,   191,   192,    30,    89,   249,   226,   260,   261,
     342,   244,   262,   246,    46,    47,   157,   218,   247,    16,
      31,    32,    97,    98,    58,   169,   170,   171
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint16 yytable[] =
{
      53,   127,    53,    68,    69,   141,   245,    17,    49,   160,
     285,   143,    14,    65,    19,   114,   235,   220,     5,    14,
     293,    13,   130,    18,    70,   301,   302,   155,    13,    23,
     265,   156,    50,   195,    24,   214,   203,    50,   274,   315,
     287,   288,   274,   294,   318,   109,    15,   228,     6,     7,
     204,    73,   275,   196,    55,    53,   283,   165,   121,   129,
      72,    20,    53,   162,   163,   166,   299,   167,   144,   145,
     146,   147,   320,   321,   334,   264,    65,   220,   160,   168,
      74,    75,   112,   103,   323,   324,   202,   112,   187,    14,
      21,   338,   211,   339,    14,   151,   143,    24,    13,   199,
     230,    25,   113,    13,     6,     7,    26,   154,   166,   113,
     167,   263,    26,    60,    61,   149,   150,    33,     6,     7,
      34,    35,   168,   110,    36,    37,    38,    62,   270,   271,
     155,   245,   245,   227,   156,   290,   291,   122,   236,   237,
     238,    27,    50,   266,   263,   289,    64,   239,   216,     6,
       7,   319,   217,   212,   213,   240,    54,   241,    57,    50,
     307,   273,   310,   311,   312,   313,   314,    48,    77,   242,
      81,    39,   132,   133,    40,    82,   263,   263,     6,     7,
     243,   325,   326,   327,   328,    33,   134,    59,    34,    35,
      33,    27,    36,    37,    38,    76,    93,    36,    37,    38,
     135,   136,   278,   279,    63,   280,   253,   236,   237,   238,
     254,   255,   256,   257,   137,    84,   239,     6,     7,    90,
     178,   179,     6,     7,   240,    91,   241,    88,    50,   104,
     105,   236,   237,   238,   180,   106,   258,    92,   242,    39,
     306,    27,    40,   139,    39,   303,   140,    40,   240,   259,
     241,    27,    50,   236,   237,   238,    28,    29,   123,   131,
     295,   296,   242,   124,    29,   128,   152,   138,   322,   148,
     240,    82,   241,   243,    50,   276,   277,   278,   279,     7,
     280,   295,   296,   153,   242,   123,   123,   316,   304,   158,
     159,   193,   317,   297,   298,   243,   276,   277,   278,   279,
     172,   280,   201,   276,   277,   278,   279,    29,   280,   115,
     116,   117,   118,   119,   297,   298,   304,   276,   277,   278,
     279,   123,   280,   272,    66,   111,   222,    67,    29,    79,
     111,    86,    80,    95,    87,   142,    96,   181,    96,   189,
      96,   210,   190,   219,    96,   164,   190,   250,   282,   184,
     190,   190,     1,     2,     3,    74,    75,    99,   100,   101,
     102,   290,   291,   308,   309,   161,   107,   108,   343,   344,
     173,   174,   175,   176,   177,   185,   183,   200,   198,   205,
     206,   215,   207,   221,   209,   208,   224,   223,   225,   229,
     232,   233,   234,   248,   281,   252,    22,    56,   267,   274,
     268,   269,   186,   286,   292,   194,   280,   251,   291,   300,
     182,   188,   305,   329,   231,   330,   331,   332,   333,   335,
       0,     0,     0,     0,     0,   336,   337,   340,   341,     0,
       0,   345,   346,    83,     0,    94
};

static const yytype_int16 yycheck[] =
{
      24,    84,    26,    34,    35,    96,   215,     3,    23,   125,
     252,    97,     1,    31,     3,    80,   214,   191,     3,     8,
      27,     1,    87,     3,    39,   267,   268,    30,     8,    50,
     228,    34,    34,     1,    55,   187,    49,    34,    49,   281,
     258,   259,    49,    50,   286,    76,    51,   199,    33,    34,
      63,    40,    63,    21,    56,    79,    59,    22,    82,    56,
      40,     0,    86,   128,   129,    30,   264,    32,    99,   100,
     101,   102,   290,   291,   316,   227,    94,   251,   194,    44,
      53,    54,    78,    56,   293,   294,   169,    83,   153,    78,
      50,   333,   183,   335,    83,   110,   182,    55,    78,   164,
      22,    50,    79,    83,    33,    34,    55,   122,    30,    86,
      32,   225,    55,    30,    31,   104,   105,     1,    33,    34,
       4,     5,    44,    52,     8,     9,    10,    44,   242,   243,
      30,   340,   341,   198,    34,    40,    41,    52,    13,    14,
      15,    55,    34,   234,   258,   259,    60,    22,    30,    33,
      34,    56,    34,   184,   185,    30,    51,    32,    30,    34,
     274,   244,   276,   277,   278,   279,   280,    51,    57,    44,
      49,    55,    30,    31,    58,    54,   290,   291,    33,    34,
      55,   295,   296,   297,   298,     1,    44,    52,     4,     5,
       1,    55,     8,     9,    10,    40,    60,     8,     9,    10,
      30,    31,    45,    46,    60,    48,    12,    13,    14,    15,
      16,    17,    18,    19,    44,    51,    22,    33,    34,    54,
      30,    31,    33,    34,    30,    54,    32,    35,    34,    53,
      54,    13,    14,    15,    44,    59,    42,    30,    44,    55,
      22,    55,    58,    52,    55,   269,    60,    58,    30,    55,
      32,    55,    34,    13,    14,    15,    60,    61,    55,    54,
      28,    29,    44,    60,    61,    26,    49,    54,   292,    57,
      30,    54,    32,    55,    34,    43,    44,    45,    46,    34,
      48,    28,    29,    26,    44,    55,    55,    54,    56,    52,
      60,    60,    59,    61,    62,    55,    43,    44,    45,    46,
      62,    48,    56,    43,    44,    45,    46,    61,    48,    16,
      17,    18,    19,    20,    61,    62,    56,    43,    44,    45,
      46,    55,    48,    56,    56,    78,    60,    59,    61,    53,
      83,    53,    56,    52,    56,    52,    55,    52,    55,    52,
      55,    52,    55,    52,    55,    26,    55,    52,    52,     7,
      55,    55,    37,    38,    39,    53,    54,     6,     7,     6,
       7,    40,    41,    24,    25,    60,    74,    75,   340,   341,
      62,    30,    62,    62,    30,     7,    54,    30,    26,    62,
      62,    54,    62,    52,    30,    62,    50,    59,    11,    56,
      24,    62,    54,    36,    54,    58,     8,    26,    55,    49,
      55,    55,   152,    55,    55,   161,    48,   222,    41,    54,
     140,   157,    56,   300,   203,    56,    56,    56,    54,    54,
      -1,    -1,    -1,    -1,    -1,    56,    56,    54,    54,    -1,
      -1,    56,    56,    54,    -1,    63
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    37,    38,    39,    65,     3,    33,    34,    66,    67,
      68,    69,    77,    98,    99,    51,   103,    77,    98,    99,
       0,    50,    67,    50,    55,    50,    55,    55,    60,    61,
      88,   104,   105,     1,     4,     5,     8,     9,    10,    55,
      58,    71,    72,    73,    74,    75,    98,    99,    51,    71,
      34,    78,    79,   102,    51,    56,    78,    30,   108,    52,
      30,    31,    44,    60,    60,   105,    56,    59,    73,    73,
      71,    76,    98,    99,    53,    54,    40,    57,    70,    53,
      56,    49,    54,    70,    51,    83,    53,    56,    35,    89,
      54,    54,    30,    60,   104,    52,    55,   106,   107,     6,
       7,     6,     7,    56,    53,    54,    59,    72,    72,    73,
      52,    69,    77,    79,    83,    16,    17,    18,    19,    20,
      82,   102,    52,    55,    60,    84,    85,    88,    26,    56,
      83,    54,    30,    31,    44,    30,    31,    44,    54,    52,
      60,   108,    52,   107,    73,    73,    73,    73,    57,    99,
      99,    71,    49,    26,    71,    30,    34,   100,    52,    60,
      85,    60,    83,    83,    26,    22,    30,    32,    44,   109,
     110,   111,    62,    62,    30,    62,    62,    30,    30,    31,
      44,    52,   106,    54,     7,     7,    82,    83,    89,    52,
      55,    86,    87,    60,    84,     1,    21,    80,    26,    83,
      30,    56,    88,    49,    63,    62,    62,    62,    62,    30,
      52,   108,    73,    73,    80,    54,    30,    34,   101,    52,
      87,    52,    60,    59,    50,    11,    91,    83,    80,    56,
      22,   111,    24,    62,    54,    91,    13,    14,    15,    22,
      30,    32,    44,    55,    95,    96,    97,   102,    36,    90,
      52,    86,    58,    12,    16,    17,    18,    19,    42,    55,
      92,    93,    96,    97,    80,    91,   108,    55,    55,    55,
      97,    97,    56,    88,    49,    63,    43,    44,    45,    46,
      48,    54,    52,    59,    81,   100,    55,    92,    92,    97,
      40,    41,    55,    27,    50,    28,    29,    61,    62,    91,
      54,   100,   100,   102,    56,    56,    22,    97,    24,    25,
      97,    97,    97,    97,    97,   100,    54,    59,   100,    56,
      92,    92,   102,    96,    96,    97,    97,    97,    97,   109,
      56,    56,    56,    54,   100,    54,    56,    56,   100,   100,
      54,    54,    94,    95,    95,    56,    56
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    64,    65,    65,    65,    66,    66,    67,    67,    67,
      68,    69,    69,    69,    70,    70,    70,    71,    71,    71,
      72,    72,    72,    72,    72,    72,    72,    72,    72,    73,
      73,    73,    73,    73,    73,    73,    73,    74,    74,    74,
      75,    75,    76,    76,    76,    76,    77,    77,    77,    77,
      78,    78,    79,    79,    80,    80,    80,    81,    81,    82,
      82,    82,    82,    82,    83,    83,    83,    83,    83,    83,
      84,    84,    85,    85,    86,    86,    87,    88,    88,    88,
      88,    88,    88,    88,    88,    88,    89,    89,    90,    90,
      91,    91,    92,    92,    92,    92,    92,    92,    92,    92,
      92,    92,    92,    92,    93,    93,    93,    93,    94,    94,
      95,    95,    95,    96,    96,    96,    96,    97,    97,    97,
      97,    97,    97,    97,    97,    97,    97,    97,    97,    97,
      98,    99,   100,   100,   101,   101,   102,   103,   103,   103,
     103,   103,   103,   104,   104,   105,   105,   106,   106,   107,
     108,   109,   109,   110,   110,   110,   110,   111,   111,   111
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     2,     2,     1,     2,     1,     1,     1,
       3,     3,     6,     6,     0,     2,     2,     1,     3,     3,
       1,     3,     4,     6,     4,     2,     4,     4,     6,     3,
       4,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       3,     2,     1,     3,     1,     3,     9,     8,     9,    10,
       3,     5,     1,     3,     4,     5,     2,     1,     3,     1,
       1,     1,     1,     1,     3,     5,     4,     6,     5,     7,
       1,     2,     6,     7,     1,     2,    10,     5,     5,     6,
       6,     7,     5,     6,     5,     6,     0,     1,     0,     1,
       0,     2,     4,     7,     3,     3,     3,     3,     3,     3,
       2,     3,     3,     3,     1,     1,     1,     1,     0,     2,
       1,     3,     3,     1,     3,     1,     3,     1,     1,     1,
       4,     4,     4,     2,     3,     3,     3,     3,     3,     3,
       1,     1,     1,     1,     1,     1,     1,     3,     5,     4,
       6,     5,     7,     1,     2,     6,     7,     1,     2,     9,
       1,     1,     3,     1,     3,     1,     3,     1,     2,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (yylocationp);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  switch (yytype)
    {
          case 32: /* STR  */
#line 149 "parser.y" /* yacc.c:1257  */
      { free(((*yyvaluep).str)); }
#line 1431 "parser.c" /* yacc.c:1257  */
        break;

    case 33: /* PROCID  */
#line 149 "parser.y" /* yacc.c:1257  */
      { free(((*yyvaluep).id)); }
#line 1437 "parser.c" /* yacc.c:1257  */
        break;

    case 34: /* ID  */
#line 149 "parser.y" /* yacc.c:1257  */
      { free(((*yyvaluep).id)); }
#line 1443 "parser.c" /* yacc.c:1257  */
        break;

    case 66: /* Program  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1449 "parser.c" /* yacc.c:1257  */
        break;

    case 67: /* Declaration  */
#line 151 "parser.y" /* yacc.c:1257  */
      { freeASTDeclaration(((*yyvaluep).decl)); }
#line 1455 "parser.c" /* yacc.c:1257  */
        break;

    case 68: /* MainDecl  */
#line 152 "parser.y" /* yacc.c:1257  */
      { freeASTCommand(((*yyvaluep).command)); }
#line 1461 "parser.c" /* yacc.c:1257  */
        break;

    case 70: /* LocalDecls  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1467 "parser.c" /* yacc.c:1257  */
        break;

    case 71: /* ComSeq  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1473 "parser.c" /* yacc.c:1257  */
        break;

    case 72: /* Command  */
#line 152 "parser.y" /* yacc.c:1257  */
      { freeASTCommand(((*yyvaluep).command)); }
#line 1479 "parser.c" /* yacc.c:1257  */
        break;

    case 73: /* Block  */
#line 152 "parser.y" /* yacc.c:1257  */
      { freeASTCommand(((*yyvaluep).command)); }
#line 1485 "parser.c" /* yacc.c:1257  */
        break;

    case 74: /* SimpleCommand  */
#line 152 "parser.y" /* yacc.c:1257  */
      { freeASTCommand(((*yyvaluep).command)); }
#line 1491 "parser.c" /* yacc.c:1257  */
        break;

    case 75: /* RuleSetCall  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1497 "parser.c" /* yacc.c:1257  */
        break;

    case 76: /* IDList  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1503 "parser.c" /* yacc.c:1257  */
        break;

    case 77: /* RuleDecl  */
#line 153 "parser.y" /* yacc.c:1257  */
      { freeASTRule(((*yyvaluep).rule)); }
#line 1509 "parser.c" /* yacc.c:1257  */
        break;

    case 78: /* VarDecls  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1515 "parser.c" /* yacc.c:1257  */
        break;

    case 79: /* VarList  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1521 "parser.c" /* yacc.c:1257  */
        break;

    case 80: /* Inter  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1527 "parser.c" /* yacc.c:1257  */
        break;

    case 81: /* NodeIDList  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1533 "parser.c" /* yacc.c:1257  */
        break;

    case 83: /* Graph  */
#line 154 "parser.y" /* yacc.c:1257  */
      { freeASTGraph(((*yyvaluep).graph)); }
#line 1539 "parser.c" /* yacc.c:1257  */
        break;

    case 84: /* NodeList  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1545 "parser.c" /* yacc.c:1257  */
        break;

    case 85: /* Node  */
#line 155 "parser.y" /* yacc.c:1257  */
      { freeASTNode(((*yyvaluep).node)); }
#line 1551 "parser.c" /* yacc.c:1257  */
        break;

    case 86: /* EdgeList  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1557 "parser.c" /* yacc.c:1257  */
        break;

    case 87: /* Edge  */
#line 156 "parser.y" /* yacc.c:1257  */
      { freeASTEdge(((*yyvaluep).edge)); }
#line 1563 "parser.c" /* yacc.c:1257  */
        break;

    case 91: /* CondDecl  */
#line 157 "parser.y" /* yacc.c:1257  */
      { freeASTCondition(((*yyvaluep).cond_exp)); }
#line 1569 "parser.c" /* yacc.c:1257  */
        break;

    case 92: /* Condition  */
#line 157 "parser.y" /* yacc.c:1257  */
      { freeASTCondition(((*yyvaluep).cond_exp)); }
#line 1575 "parser.c" /* yacc.c:1257  */
        break;

    case 94: /* LabelArg  */
#line 158 "parser.y" /* yacc.c:1257  */
      { freeASTLabel(((*yyvaluep).label)); }
#line 1581 "parser.c" /* yacc.c:1257  */
        break;

    case 95: /* Label  */
#line 158 "parser.y" /* yacc.c:1257  */
      { freeASTLabel(((*yyvaluep).label)); }
#line 1587 "parser.c" /* yacc.c:1257  */
        break;

    case 96: /* List  */
#line 150 "parser.y" /* yacc.c:1257  */
      { freeAST(((*yyvaluep).list)); }
#line 1593 "parser.c" /* yacc.c:1257  */
        break;

    case 97: /* AtomExp  */
#line 159 "parser.y" /* yacc.c:1257  */
      { freeASTAtom(((*yyvaluep).atom_exp)); }
#line 1599 "parser.c" /* yacc.c:1257  */
        break;

    case 98: /* ProcID  */
#line 149 "parser.y" /* yacc.c:1257  */
      { free(((*yyvaluep).id)); }
#line 1605 "parser.c" /* yacc.c:1257  */
        break;

    case 99: /* RuleID  */
#line 149 "parser.y" /* yacc.c:1257  */
      { free(((*yyvaluep).id)); }
#line 1611 "parser.c" /* yacc.c:1257  */
        break;

    case 100: /* NodeID  */
#line 149 "parser.y" /* yacc.c:1257  */
      { free(((*yyvaluep).id)); }
#line 1617 "parser.c" /* yacc.c:1257  */
        break;

    case 101: /* EdgeID  */
#line 149 "parser.y" /* yacc.c:1257  */
      { free(((*yyvaluep).id)); }
#line 1623 "parser.c" /* yacc.c:1257  */
        break;

    case 102: /* Variable  */
#line 149 "parser.y" /* yacc.c:1257  */
      { free(((*yyvaluep).id)); }
#line 1629 "parser.c" /* yacc.c:1257  */
        break;


      default:
        break;
    }
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  yylsp[0] = yylloc;
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yyls1, yysize * sizeof (*yylsp),
                    &yystacksize);

        yyls = yyls1;
        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 220 "parser.y" /* yacc.c:1646  */
    { gp_program = (yyvsp[0].list); }
#line 1917 "parser.c" /* yacc.c:1646  */
    break;

  case 3:
#line 221 "parser.y" /* yacc.c:1646  */
    { }
#line 1923 "parser.c" /* yacc.c:1646  */
    break;

  case 4:
#line 222 "parser.y" /* yacc.c:1646  */
    { if ((yyvsp[0].rule)) freeASTRule((yyvsp[0].rule)); }
#line 1929 "parser.c" /* yacc.c:1646  */
    break;

  case 5:
#line 225 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTDecl(GLOBAL_DECLARATIONS, 
                                               (yylsp[0]), (yyvsp[0].decl), NULL); }
#line 1936 "parser.c" /* yacc.c:1646  */
    break;

  case 6:
#line 227 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTDecl(GLOBAL_DECLARATIONS, 
                                               (yylsp[0]), (yyvsp[0].decl), (yyvsp[-1].list)); }
#line 1943 "parser.c" /* yacc.c:1646  */
    break;

  case 7:
#line 230 "parser.y" /* yacc.c:1646  */
    { (yyval.decl) = newASTMainDecl((yyloc), (yyvsp[0].command)); }
#line 1949 "parser.c" /* yacc.c:1646  */
    break;

  case 8:
#line 231 "parser.y" /* yacc.c:1646  */
    { (yyval.decl) = newASTProcedureDecl((yyloc), (yyvsp[0].proc)); }
#line 1955 "parser.c" /* yacc.c:1646  */
    break;

  case 9:
#line 232 "parser.y" /* yacc.c:1646  */
    { (yyval.decl) = newASTRuleDecl((yyloc), (yyvsp[0].rule)); }
#line 1961 "parser.c" /* yacc.c:1646  */
    break;

  case 10:
#line 234 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTCommandSequence((yylsp[-2]), (yyvsp[0].list)); }
#line 1967 "parser.c" /* yacc.c:1646  */
    break;

  case 11:
#line 236 "parser.y" /* yacc.c:1646  */
    { (yyval.proc) = newASTProcedure((yylsp[-2]), (yyvsp[-2].id), NULL, 
                                               newASTCommandSequence((yylsp[0]) ,(yyvsp[0].list)));
					  if((yyvsp[-2].id)) free((yyvsp[-2].id)); }
#line 1975 "parser.c" /* yacc.c:1646  */
    break;

  case 12:
#line 241 "parser.y" /* yacc.c:1646  */
    { (yyval.proc) = newASTProcedure((yylsp[-5]), (yyvsp[-5].id), (yyvsp[-2].list), 
                                               newASTCommandSequence((yylsp[0]), (yyvsp[0].list)));
				          if((yyvsp[-5].id)) free((yyvsp[-5].id)); }
#line 1983 "parser.c" /* yacc.c:1646  */
    break;

  case 13:
#line 246 "parser.y" /* yacc.c:1646  */
    { (yyval.proc) = newASTProcedure((yylsp[-5]), (yyvsp[-5].id), (yyvsp[-2].list), 
                                               newASTCommandSequence((yylsp[0]), (yyvsp[0].list)));
                                          report_warning("Procedure names must "
 					   "start with an upper-case letter."); 
					  if((yyvsp[-5].id)) free((yyvsp[-5].id)); }
#line 1993 "parser.c" /* yacc.c:1646  */
    break;

  case 14:
#line 252 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = NULL; }
#line 1999 "parser.c" /* yacc.c:1646  */
    break;

  case 15:
#line 253 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTDecl(LOCAL_DECLARATIONS, (yylsp[0]), 
                                               newASTRuleDecl((yylsp[0]), (yyvsp[0].rule)), (yyvsp[-1].list)); }
#line 2006 "parser.c" /* yacc.c:1646  */
    break;

  case 16:
#line 255 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTDecl(LOCAL_DECLARATIONS, (yylsp[0]),
                                               newASTProcedureDecl((yylsp[0]), (yyvsp[0].proc)), (yyvsp[-1].list)); }
#line 2013 "parser.c" /* yacc.c:1646  */
    break;

  case 17:
#line 258 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTCommand((yylsp[0]), (yyvsp[0].command), NULL); }
#line 2019 "parser.c" /* yacc.c:1646  */
    break;

  case 18:
#line 259 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTCommand((yylsp[0]), (yyvsp[0].command), (yyvsp[-2].list)); }
#line 2025 "parser.c" /* yacc.c:1646  */
    break;

  case 19:
#line 261 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTCommand((yylsp[0]), (yyvsp[0].command), (yyvsp[-2].list));
                                          report_warning("Incorrect use of comma "
					    "to separate commands. Perhaps you "
					    "meant to use a semicolon?"); }
#line 2034 "parser.c" /* yacc.c:1646  */
    break;

  case 21:
#line 267 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTOrStmt((yyloc), (yyvsp[-2].command), (yyvsp[0].command)); }
#line 2040 "parser.c" /* yacc.c:1646  */
    break;

  case 22:
#line 268 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTCondBranch(IF_STATEMENT, (yyloc),
                                               (yyvsp[-2].command), (yyvsp[0].command), newASTSkip((yyloc))); }
#line 2047 "parser.c" /* yacc.c:1646  */
    break;

  case 23:
#line 270 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTCondBranch(IF_STATEMENT, (yyloc),
                                               (yyvsp[-4].command), (yyvsp[-2].command), (yyvsp[0].command)); }
#line 2054 "parser.c" /* yacc.c:1646  */
    break;

  case 24:
#line 273 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTCondBranch(IF_STATEMENT, (yyloc),
                                               (yyvsp[-2].command), newASTSkip((yyloc)), (yyvsp[0].command));
                                          report_warning("No 'then' clause in if "
						         "statement."); }
#line 2063 "parser.c" /* yacc.c:1646  */
    break;

  case 25:
#line 277 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTCondBranch(TRY_STATEMENT, (yyloc),
                                               (yyvsp[0].command), newASTSkip((yyloc)), newASTSkip((yyloc))); }
#line 2070 "parser.c" /* yacc.c:1646  */
    break;

  case 26:
#line 279 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTCondBranch(TRY_STATEMENT, (yyloc),
                                               (yyvsp[-2].command), (yyvsp[0].command), newASTSkip((yyloc))); }
#line 2077 "parser.c" /* yacc.c:1646  */
    break;

  case 27:
#line 281 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTCondBranch(TRY_STATEMENT, (yyloc),
                                               (yyvsp[-2].command), newASTSkip((yyloc)), (yyvsp[0].command)); }
#line 2084 "parser.c" /* yacc.c:1646  */
    break;

  case 28:
#line 283 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTCondBranch(TRY_STATEMENT, (yyloc),
                                                (yyvsp[-4].command), (yyvsp[-2].command), (yyvsp[0].command)); }
#line 2091 "parser.c" /* yacc.c:1646  */
    break;

  case 29:
#line 287 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTCommandSequence((yyloc), (yyvsp[-1].list)); }
#line 2097 "parser.c" /* yacc.c:1646  */
    break;

  case 30:
#line 288 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTAlap((yyloc), 
                                               newASTCommandSequence((yylsp[-2]), (yyvsp[-2].list))); }
#line 2104 "parser.c" /* yacc.c:1646  */
    break;

  case 31:
#line 292 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = NULL; }
#line 2110 "parser.c" /* yacc.c:1646  */
    break;

  case 33:
#line 294 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTAlap((yyloc), (yyvsp[-1].command)); }
#line 2116 "parser.c" /* yacc.c:1646  */
    break;

  case 34:
#line 295 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTSkip((yyloc)); }
#line 2122 "parser.c" /* yacc.c:1646  */
    break;

  case 35:
#line 296 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTFail((yyloc)); }
#line 2128 "parser.c" /* yacc.c:1646  */
    break;

  case 36:
#line 297 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTBreak((yyloc)); }
#line 2134 "parser.c" /* yacc.c:1646  */
    break;

  case 37:
#line 299 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTRuleSetCall((yyloc), (yyvsp[0].list)); }
#line 2140 "parser.c" /* yacc.c:1646  */
    break;

  case 38:
#line 300 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTRuleCall((yyloc), (yyvsp[0].id)); if((yyvsp[0].id)) free((yyvsp[0].id)); }
#line 2146 "parser.c" /* yacc.c:1646  */
    break;

  case 39:
#line 301 "parser.y" /* yacc.c:1646  */
    { (yyval.command) = newASTProcCall((yyloc), (yyvsp[0].id)); if((yyvsp[0].id)) free((yyvsp[0].id)); }
#line 2152 "parser.c" /* yacc.c:1646  */
    break;

  case 40:
#line 303 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = (yyvsp[-1].list); }
#line 2158 "parser.c" /* yacc.c:1646  */
    break;

  case 41:
#line 306 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = NULL; }
#line 2164 "parser.c" /* yacc.c:1646  */
    break;

  case 42:
#line 308 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTRule((yylsp[0]), (yyvsp[0].id), NULL);
					  if((yyvsp[0].id)) free((yyvsp[0].id)); }
#line 2171 "parser.c" /* yacc.c:1646  */
    break;

  case 43:
#line 310 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTRule((yylsp[0]), (yyvsp[0].id), (yyvsp[-2].list)); 
					  if((yyvsp[0].id)) free((yyvsp[0].id));}
#line 2178 "parser.c" /* yacc.c:1646  */
    break;

  case 44:
#line 313 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTRule((yylsp[0]), (yyvsp[0].id), NULL);
                                          report_warning("Procedure name used in "
					   "a rule set. Rule names must start "
					   "with a lower-case letter.");
				          if((yyvsp[0].id)) free((yyvsp[0].id)); }
#line 2188 "parser.c" /* yacc.c:1646  */
    break;

  case 45:
#line 318 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTRule((yylsp[0]), (yyvsp[0].id), (yyvsp[-2].list));
                                          report_warning("Incorrect use of semicolon "
					   "in a rule set. Perhaps you meant to "
					   "use a comma?"); 
					  if((yyvsp[0].id)) free((yyvsp[0].id)); }
#line 2198 "parser.c" /* yacc.c:1646  */
    break;

  case 46:
#line 327 "parser.y" /* yacc.c:1646  */
    { (yyval.rule) = newASTRule((yylsp[-8]), (yyvsp[-8].id), (yyvsp[-6].list), (yyvsp[-4].graph), (yyvsp[-2].graph), (yyvsp[-1].list), (yyvsp[0].cond_exp)); 
					  if((yyvsp[-8].id)) free((yyvsp[-8].id)); }
#line 2205 "parser.c" /* yacc.c:1646  */
    break;

  case 47:
#line 330 "parser.y" /* yacc.c:1646  */
    { (yyval.rule) = newASTRule((yylsp[-7]), (yyvsp[-7].id), NULL, (yyvsp[-4].graph), (yyvsp[-2].graph), (yyvsp[-1].list), (yyvsp[0].cond_exp));
					  if((yyvsp[-7].id)) free((yyvsp[-7].id)); }
#line 2212 "parser.c" /* yacc.c:1646  */
    break;

  case 48:
#line 334 "parser.y" /* yacc.c:1646  */
    { (yyval.rule) = newASTRule((yylsp[-8]), (yyvsp[-8].id), (yyvsp[-6].list), (yyvsp[-4].graph), (yyvsp[-2].graph), (yyvsp[-1].list), (yyvsp[0].cond_exp)); 
                                          report_warning("Rule names must "
 					   "start with a lower-case letter."
				 	   "letter.");
					  if((yyvsp[-8].id)) free((yyvsp[-8].id)); }
#line 2222 "parser.c" /* yacc.c:1646  */
    break;

  case 49:
#line 343 "parser.y" /* yacc.c:1646  */
    { (yyval.rule) = newASTRule((yylsp[-9]), (yyvsp[-9].id), (yyvsp[-7].list), (yyvsp[-4].graph), (yyvsp[-2].graph), (yyvsp[-1].list), (yyvsp[0].cond_exp));  
                                          report_warning("Semicolon at the end "
					    "of a rule's variable list");
					  if((yyvsp[-9].id)) free((yyvsp[-9].id)); }
#line 2231 "parser.c" /* yacc.c:1646  */
    break;

  case 50:
#line 349 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTVariableDecl((yyvsp[0].list_type), (yyloc), (yyvsp[-2].list), NULL); }
#line 2237 "parser.c" /* yacc.c:1646  */
    break;

  case 51:
#line 354 "parser.y" /* yacc.c:1646  */
    { (yyloc).first_column = (yylsp[-2]).first_column;
				          (yyloc).first_line = (yylsp[-2]).first_line;
					  (yyloc).last_column = (yylsp[0]).last_column;
				          (yyloc).last_column = (yylsp[0]).last_column;
					  (yyval.list) = addASTVariableDecl((yyvsp[0].list_type), (yyloc), (yyvsp[-2].list), (yyvsp[-4].list)); }
#line 2247 "parser.c" /* yacc.c:1646  */
    break;

  case 52:
#line 360 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTVariable((yylsp[0]), (yyvsp[0].id), NULL); 
					  if((yyvsp[0].id)) free((yyvsp[0].id)); }
#line 2254 "parser.c" /* yacc.c:1646  */
    break;

  case 53:
#line 362 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTVariable((yylsp[0]), (yyvsp[0].id), (yyvsp[-2].list)); 
		 	                  if((yyvsp[0].id)) free((yyvsp[0].id)); }
#line 2261 "parser.c" /* yacc.c:1646  */
    break;

  case 54:
#line 365 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = NULL; }
#line 2267 "parser.c" /* yacc.c:1646  */
    break;

  case 55:
#line 366 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = (yyvsp[-1].list); }
#line 2273 "parser.c" /* yacc.c:1646  */
    break;

  case 56:
#line 369 "parser.y" /* yacc.c:1646  */
    { report_warning("Error in an interface list.");  
                                          (yyval.list) = NULL; }
#line 2280 "parser.c" /* yacc.c:1646  */
    break;

  case 57:
#line 372 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTNodeID((yylsp[0]), (yyvsp[0].id), NULL); 
					  if((yyvsp[0].id)) free((yyvsp[0].id)); }
#line 2287 "parser.c" /* yacc.c:1646  */
    break;

  case 58:
#line 374 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTNodeID((yylsp[0]), (yyvsp[0].id), (yyvsp[-2].list));
					  if((yyvsp[0].id)) free((yyvsp[0].id)); }
#line 2294 "parser.c" /* yacc.c:1646  */
    break;

  case 59:
#line 377 "parser.y" /* yacc.c:1646  */
    { (yyval.list_type) = INT_DECLARATIONS; }
#line 2300 "parser.c" /* yacc.c:1646  */
    break;

  case 60:
#line 378 "parser.y" /* yacc.c:1646  */
    { (yyval.list_type) = CHAR_DECLARATIONS; }
#line 2306 "parser.c" /* yacc.c:1646  */
    break;

  case 61:
#line 379 "parser.y" /* yacc.c:1646  */
    { (yyval.list_type) = STRING_DECLARATIONS; }
#line 2312 "parser.c" /* yacc.c:1646  */
    break;

  case 62:
#line 380 "parser.y" /* yacc.c:1646  */
    { (yyval.list_type) = ATOM_DECLARATIONS; }
#line 2318 "parser.c" /* yacc.c:1646  */
    break;

  case 63:
#line 381 "parser.y" /* yacc.c:1646  */
    { (yyval.list_type) = LIST_DECLARATIONS; }
#line 2324 "parser.c" /* yacc.c:1646  */
    break;

  case 64:
#line 385 "parser.y" /* yacc.c:1646  */
    { (yyval.graph) = newASTGraph((yyloc), NULL, NULL); }
#line 2330 "parser.c" /* yacc.c:1646  */
    break;

  case 65:
#line 386 "parser.y" /* yacc.c:1646  */
    { (yyval.graph) = newASTGraph((yyloc), NULL, NULL); }
#line 2336 "parser.c" /* yacc.c:1646  */
    break;

  case 66:
#line 387 "parser.y" /* yacc.c:1646  */
    { (yyval.graph) = newASTGraph((yyloc), (yyvsp[-2].list), NULL); }
#line 2342 "parser.c" /* yacc.c:1646  */
    break;

  case 67:
#line 388 "parser.y" /* yacc.c:1646  */
    { (yyval.graph) = newASTGraph((yyloc), (yyvsp[-2].list), NULL); }
#line 2348 "parser.c" /* yacc.c:1646  */
    break;

  case 68:
#line 389 "parser.y" /* yacc.c:1646  */
    { (yyval.graph) = newASTGraph((yyloc), (yyvsp[-3].list), (yyvsp[-1].list)); }
#line 2354 "parser.c" /* yacc.c:1646  */
    break;

  case 69:
#line 391 "parser.y" /* yacc.c:1646  */
    { (yyval.graph) = newASTGraph((yyloc), (yyvsp[-3].list), (yyvsp[-1].list)); }
#line 2360 "parser.c" /* yacc.c:1646  */
    break;

  case 70:
#line 393 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTNode((yylsp[0]), (yyvsp[0].node), NULL); }
#line 2366 "parser.c" /* yacc.c:1646  */
    break;

  case 71:
#line 394 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTNode((yylsp[0]), (yyvsp[0].node), (yyvsp[-1].list)); }
#line 2372 "parser.c" /* yacc.c:1646  */
    break;

  case 72:
#line 396 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = newASTNode((yylsp[-4]), is_root, (yyvsp[-4].id), (yyvsp[-1].label)); 
 					  is_root = false; 	
					  if((yyvsp[-4].id)) free((yyvsp[-4].id)); }
#line 2380 "parser.c" /* yacc.c:1646  */
    break;

  case 73:
#line 400 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = newASTNode((yylsp[-5]), is_root, (yyvsp[-5].id), (yyvsp[-2].label)); 
 					  is_root = false; 	
					  if((yyvsp[-5].id)) free((yyvsp[-5].id)); }
#line 2388 "parser.c" /* yacc.c:1646  */
    break;

  case 74:
#line 403 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTEdge((yylsp[0]), (yyvsp[0].edge), NULL); }
#line 2394 "parser.c" /* yacc.c:1646  */
    break;

  case 75:
#line 404 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTEdge((yylsp[0]), (yyvsp[0].edge), (yyvsp[-1].list)); }
#line 2400 "parser.c" /* yacc.c:1646  */
    break;

  case 76:
#line 407 "parser.y" /* yacc.c:1646  */
    { (yyval.edge) = newASTEdge((yylsp[-8]), is_bidir, (yyvsp[-8].id), (yyvsp[-5].id), (yyvsp[-3].id), (yyvsp[-1].label));
                                          is_bidir = false; if((yyvsp[-8].id)) free((yyvsp[-8].id)); 
					  if((yyvsp[-5].id)) free((yyvsp[-5].id)); if((yyvsp[-3].id)) free((yyvsp[-3].id)); }
#line 2408 "parser.c" /* yacc.c:1646  */
    break;

  case 77:
#line 412 "parser.y" /* yacc.c:1646  */
    { }
#line 2414 "parser.c" /* yacc.c:1646  */
    break;

  case 78:
#line 413 "parser.y" /* yacc.c:1646  */
    { }
#line 2420 "parser.c" /* yacc.c:1646  */
    break;

  case 79:
#line 414 "parser.y" /* yacc.c:1646  */
    { }
#line 2426 "parser.c" /* yacc.c:1646  */
    break;

  case 80:
#line 415 "parser.y" /* yacc.c:1646  */
    { }
#line 2432 "parser.c" /* yacc.c:1646  */
    break;

  case 81:
#line 416 "parser.y" /* yacc.c:1646  */
    { }
#line 2438 "parser.c" /* yacc.c:1646  */
    break;

  case 82:
#line 417 "parser.y" /* yacc.c:1646  */
    { }
#line 2444 "parser.c" /* yacc.c:1646  */
    break;

  case 83:
#line 418 "parser.y" /* yacc.c:1646  */
    { }
#line 2450 "parser.c" /* yacc.c:1646  */
    break;

  case 84:
#line 419 "parser.y" /* yacc.c:1646  */
    { }
#line 2456 "parser.c" /* yacc.c:1646  */
    break;

  case 85:
#line 420 "parser.y" /* yacc.c:1646  */
    { }
#line 2462 "parser.c" /* yacc.c:1646  */
    break;

  case 87:
#line 423 "parser.y" /* yacc.c:1646  */
    { is_root = true; }
#line 2468 "parser.c" /* yacc.c:1646  */
    break;

  case 89:
#line 426 "parser.y" /* yacc.c:1646  */
    { is_bidir = true; }
#line 2474 "parser.c" /* yacc.c:1646  */
    break;

  case 90:
#line 430 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = NULL; }
#line 2480 "parser.c" /* yacc.c:1646  */
    break;

  case 91:
#line 431 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = (yyvsp[0].cond_exp); }
#line 2486 "parser.c" /* yacc.c:1646  */
    break;

  case 92:
#line 433 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTSubtypePred((yyvsp[-3].check_type), (yyloc), (yyvsp[-1].id)); 
					  if((yyvsp[-1].id)) free((yyvsp[-1].id)); }
#line 2493 "parser.c" /* yacc.c:1646  */
    break;

  case 93:
#line 436 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTEdgePred((yyloc), (yyvsp[-4].id), (yyvsp[-2].id), (yyvsp[-1].label)); 
					  if((yyvsp[-4].id)) free((yyvsp[-4].id)); if((yyvsp[-2].id)) free((yyvsp[-2].id)); }
#line 2500 "parser.c" /* yacc.c:1646  */
    break;

  case 94:
#line 438 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTListComparison(EQUAL, (yyloc), (yyvsp[-2].list), (yyvsp[0].list)); }
#line 2506 "parser.c" /* yacc.c:1646  */
    break;

  case 95:
#line 439 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTListComparison(NOT_EQUAL, (yyloc), (yyvsp[-2].list), (yyvsp[0].list)); }
#line 2512 "parser.c" /* yacc.c:1646  */
    break;

  case 96:
#line 440 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTAtomComparison(GREATER, (yyloc), (yyvsp[-2].atom_exp), (yyvsp[0].atom_exp)); }
#line 2518 "parser.c" /* yacc.c:1646  */
    break;

  case 97:
#line 441 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTAtomComparison(GREATER_EQUAL, (yyloc), (yyvsp[-2].atom_exp), (yyvsp[0].atom_exp)); }
#line 2524 "parser.c" /* yacc.c:1646  */
    break;

  case 98:
#line 442 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTAtomComparison(LESS, (yyloc), (yyvsp[-2].atom_exp), (yyvsp[0].atom_exp)); }
#line 2530 "parser.c" /* yacc.c:1646  */
    break;

  case 99:
#line 443 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTAtomComparison(LESS_EQUAL, (yyloc), (yyvsp[-2].atom_exp), (yyvsp[0].atom_exp)); }
#line 2536 "parser.c" /* yacc.c:1646  */
    break;

  case 100:
#line 444 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTNotExp((yyloc), (yyvsp[0].cond_exp)); }
#line 2542 "parser.c" /* yacc.c:1646  */
    break;

  case 101:
#line 445 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTBinaryExp(BOOL_OR, (yyloc), (yyvsp[-2].cond_exp), (yyvsp[0].cond_exp)); }
#line 2548 "parser.c" /* yacc.c:1646  */
    break;

  case 102:
#line 446 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = newASTBinaryExp(BOOL_AND, (yyloc), (yyvsp[-2].cond_exp), (yyvsp[0].cond_exp)); }
#line 2554 "parser.c" /* yacc.c:1646  */
    break;

  case 103:
#line 447 "parser.y" /* yacc.c:1646  */
    { (yyval.cond_exp) = (yyvsp[-1].cond_exp); }
#line 2560 "parser.c" /* yacc.c:1646  */
    break;

  case 104:
#line 449 "parser.y" /* yacc.c:1646  */
    { (yyval.check_type) = INT_CHECK; }
#line 2566 "parser.c" /* yacc.c:1646  */
    break;

  case 105:
#line 450 "parser.y" /* yacc.c:1646  */
    { (yyval.check_type) = CHAR_CHECK; }
#line 2572 "parser.c" /* yacc.c:1646  */
    break;

  case 106:
#line 451 "parser.y" /* yacc.c:1646  */
    { (yyval.check_type) = STRING_CHECK; }
#line 2578 "parser.c" /* yacc.c:1646  */
    break;

  case 107:
#line 452 "parser.y" /* yacc.c:1646  */
    { (yyval.check_type) = ATOM_CHECK; }
#line 2584 "parser.c" /* yacc.c:1646  */
    break;

  case 108:
#line 454 "parser.y" /* yacc.c:1646  */
    { (yyval.label) = NULL; }
#line 2590 "parser.c" /* yacc.c:1646  */
    break;

  case 109:
#line 455 "parser.y" /* yacc.c:1646  */
    { (yyval.label) = (yyvsp[0].label); }
#line 2596 "parser.c" /* yacc.c:1646  */
    break;

  case 110:
#line 458 "parser.y" /* yacc.c:1646  */
    { (yyval.label) = newASTLabel((yyloc), NONE, (yyvsp[0].list)); }
#line 2602 "parser.c" /* yacc.c:1646  */
    break;

  case 111:
#line 459 "parser.y" /* yacc.c:1646  */
    { (yyval.label) = newASTLabel((yyloc), (yyvsp[0].mark), (yyvsp[-2].list)); }
#line 2608 "parser.c" /* yacc.c:1646  */
    break;

  case 112:
#line 461 "parser.y" /* yacc.c:1646  */
    { (yyval.label) = newASTLabel((yyloc), (yyvsp[0].mark), (yyvsp[-2].list)); }
#line 2614 "parser.c" /* yacc.c:1646  */
    break;

  case 113:
#line 464 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTAtom((yylsp[0]), (yyvsp[0].atom_exp), NULL); }
#line 2620 "parser.c" /* yacc.c:1646  */
    break;

  case 114:
#line 465 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = addASTAtom((yylsp[0]), (yyvsp[0].atom_exp), (yyvsp[-2].list)); }
#line 2626 "parser.c" /* yacc.c:1646  */
    break;

  case 115:
#line 466 "parser.y" /* yacc.c:1646  */
    { (yyval.list) = NULL; }
#line 2632 "parser.c" /* yacc.c:1646  */
    break;

  case 117:
#line 469 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTVariable((yyloc), (yyvsp[0].id)); if((yyvsp[0].id)) free((yyvsp[0].id)); }
#line 2638 "parser.c" /* yacc.c:1646  */
    break;

  case 118:
#line 470 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTNumber((yyloc), (yyvsp[0].num)); }
#line 2644 "parser.c" /* yacc.c:1646  */
    break;

  case 119:
#line 471 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTString((yyloc), (yyvsp[0].str)); if((yyvsp[0].str)) free((yyvsp[0].str)); }
#line 2650 "parser.c" /* yacc.c:1646  */
    break;

  case 120:
#line 472 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTDegreeOp(INDEGREE, (yyloc), (yyvsp[-1].id)); 
					  if((yyvsp[-1].id)) free((yyvsp[-1].id)); }
#line 2657 "parser.c" /* yacc.c:1646  */
    break;

  case 121:
#line 474 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTDegreeOp(OUTDEGREE, (yyloc), (yyvsp[-1].id)); 
				 	  if((yyvsp[-1].id)) free((yyvsp[-1].id)); }
#line 2664 "parser.c" /* yacc.c:1646  */
    break;

  case 122:
#line 476 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTLength((yyloc), (yyvsp[-1].id)); if((yyvsp[-1].id)) free((yyvsp[-1].id)); }
#line 2670 "parser.c" /* yacc.c:1646  */
    break;

  case 123:
#line 477 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTNegExp((yyloc), (yyvsp[0].atom_exp)); }
#line 2676 "parser.c" /* yacc.c:1646  */
    break;

  case 124:
#line 478 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = (yyvsp[-1].atom_exp); }
#line 2682 "parser.c" /* yacc.c:1646  */
    break;

  case 125:
#line 479 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTBinaryOp(ADD, (yyloc), (yyvsp[-2].atom_exp), (yyvsp[0].atom_exp));  }
#line 2688 "parser.c" /* yacc.c:1646  */
    break;

  case 126:
#line 480 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTBinaryOp(SUBTRACT, (yyloc), (yyvsp[-2].atom_exp), (yyvsp[0].atom_exp)); }
#line 2694 "parser.c" /* yacc.c:1646  */
    break;

  case 127:
#line 481 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTBinaryOp(MULTIPLY, (yyloc), (yyvsp[-2].atom_exp), (yyvsp[0].atom_exp)); }
#line 2700 "parser.c" /* yacc.c:1646  */
    break;

  case 128:
#line 482 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTBinaryOp(DIVIDE, (yyloc), (yyvsp[-2].atom_exp), (yyvsp[0].atom_exp)); }
#line 2706 "parser.c" /* yacc.c:1646  */
    break;

  case 129:
#line 483 "parser.y" /* yacc.c:1646  */
    { (yyval.atom_exp) = newASTConcat((yyloc), (yyvsp[-2].atom_exp), (yyvsp[0].atom_exp)); }
#line 2712 "parser.c" /* yacc.c:1646  */
    break;

  case 133:
#line 489 "parser.y" /* yacc.c:1646  */
    { char id[64]; int write;
					  write = snprintf(id, 64, "%d", (yyvsp[0].num));
				          if(write < 0) {
					    yyerror("Node ID conversion failed.");
					    exit(1);
					  }
					  else (yyval.id) = strdup(id);
					}
#line 2725 "parser.c" /* yacc.c:1646  */
    break;

  case 135:
#line 498 "parser.y" /* yacc.c:1646  */
    { char id[64]; int write;
					  write = snprintf(id, 64, "%d", (yyvsp[0].num));
				          if(write < 0) {
					    yyerror("Edge ID conversion failed.");
					    exit(1);
					  }
					  else (yyval.id) = strdup(id);
					}
#line 2738 "parser.c" /* yacc.c:1646  */
    break;

  case 137:
#line 510 "parser.y" /* yacc.c:1646  */
    { }
#line 2744 "parser.c" /* yacc.c:1646  */
    break;

  case 138:
#line 511 "parser.y" /* yacc.c:1646  */
    { }
#line 2750 "parser.c" /* yacc.c:1646  */
    break;

  case 139:
#line 512 "parser.y" /* yacc.c:1646  */
    { }
#line 2756 "parser.c" /* yacc.c:1646  */
    break;

  case 140:
#line 514 "parser.y" /* yacc.c:1646  */
    { }
#line 2762 "parser.c" /* yacc.c:1646  */
    break;

  case 141:
#line 516 "parser.y" /* yacc.c:1646  */
    { }
#line 2768 "parser.c" /* yacc.c:1646  */
    break;

  case 142:
#line 518 "parser.y" /* yacc.c:1646  */
    { }
#line 2774 "parser.c" /* yacc.c:1646  */
    break;

  case 143:
#line 520 "parser.y" /* yacc.c:1646  */
    { }
#line 2780 "parser.c" /* yacc.c:1646  */
    break;

  case 144:
#line 521 "parser.y" /* yacc.c:1646  */
    { }
#line 2786 "parser.c" /* yacc.c:1646  */
    break;

  case 147:
#line 528 "parser.y" /* yacc.c:1646  */
    { }
#line 2792 "parser.c" /* yacc.c:1646  */
    break;

  case 148:
#line 529 "parser.y" /* yacc.c:1646  */
    { }
#line 2798 "parser.c" /* yacc.c:1646  */
    break;

  case 151:
#line 536 "parser.y" /* yacc.c:1646  */
    { }
#line 2804 "parser.c" /* yacc.c:1646  */
    break;

  case 152:
#line 537 "parser.y" /* yacc.c:1646  */
    { }
#line 2810 "parser.c" /* yacc.c:1646  */
    break;

  case 153:
#line 539 "parser.y" /* yacc.c:1646  */
    { }
#line 2816 "parser.c" /* yacc.c:1646  */
    break;

  case 154:
#line 540 "parser.y" /* yacc.c:1646  */
    { }
#line 2822 "parser.c" /* yacc.c:1646  */
    break;

  case 155:
#line 541 "parser.y" /* yacc.c:1646  */
    { }
#line 2828 "parser.c" /* yacc.c:1646  */
    break;

  case 156:
#line 542 "parser.y" /* yacc.c:1646  */
    { }
#line 2834 "parser.c" /* yacc.c:1646  */
    break;

  case 157:
#line 544 "parser.y" /* yacc.c:1646  */
    { }
#line 2840 "parser.c" /* yacc.c:1646  */
    break;

  case 158:
#line 545 "parser.y" /* yacc.c:1646  */
    { }
#line 2846 "parser.c" /* yacc.c:1646  */
    break;

  case 159:
#line 546 "parser.y" /* yacc.c:1646  */
    { if((yyvsp[0].str)) free((yyvsp[0].str)); }
#line 2852 "parser.c" /* yacc.c:1646  */
    break;


#line 2856 "parser.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[1] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 548 "parser.y" /* yacc.c:1906  */


/* Bison calls yyerror whenever it encounters an error. It prints error
 * messages to stderr and log_file. */
void yyerror(const char *error_message)
{
   fprintf(stderr, "Error at '%c': %s\n\n", yychar, error_message);
   fprintf(log_file, "%d.%d-%d.%d: Error at '%s': %s\n\n", 
           yylloc.first_line, yylloc.first_column, yylloc.last_line, 
           yylloc.last_column, yytext, error_message);
}

/* report_warning is identical to yyerror except that it doesn't refer to yytext.
 * This is called in the action code of error-catching Bison rules in which
 * the value of yytext may be misleading. */
void report_warning(const char *error_message)
{
   fprintf(stderr, "Error: %s\n\n", error_message);
   fprintf(log_file, "%d.%d-%d.%d: Error: %s\n\n", 
           yylloc.first_line, yylloc.first_column, yylloc.last_line, 
           yylloc.last_column, error_message);
   syntax_error = true;
}
        
