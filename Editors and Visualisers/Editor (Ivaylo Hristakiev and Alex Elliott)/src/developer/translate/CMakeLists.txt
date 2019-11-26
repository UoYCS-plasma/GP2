#
# This builds the libTranslate  - a shared library containing utility functions for translating a textual GP graph into an AST structure
#

set(libTranslate_C_SRCS 
   ${GP2_SOURCE_DIR}/src/developer/translate/gplex.c
  ${GP2_SOURCE_DIR}/src/developer/translate/parser.c
  ${GP2_SOURCE_DIR}/src/developer/translate/ast.c
  ${GP2_SOURCE_DIR}/src/developer/translate/pretty.c
  ${GP2_SOURCE_DIR}/src/developer/translate/prettyGraph.c
)

add_library(libTranslate ${libTranslate_C_SRCS})
