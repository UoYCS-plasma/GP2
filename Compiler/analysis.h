/* ////////////////////////////////////////////////////////////////////////////

  ======================
  Static Analysis Module  
  ======================                      
 
  Functions to analyse and annotate the AST of a GP2 program to support
  code generation.

//////////////////////////////////////////////////////////////////////////// */

#ifndef INC_ANALYSIS_H
#define INC_ANALYSIS_H

#include "ast.h"
#include "globals.h"
#include "pretty.h"

void staticAnalysis(List *declarations, bool debug, string prefix);

typedef enum {NO_COPY = 0, RECORD_CHANGES, COPY} copyType;

void annotate(GPCommand *command, int restore_point);
copyType getCommandType(GPCommand *command, bool if_body, int com_seq,
                        bool last_command);
copyType getSequenceType(List *commands, bool if_body, int com_seq);

#endif /* INC_ANALYSIS_H */
