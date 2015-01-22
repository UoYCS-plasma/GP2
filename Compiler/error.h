/* ///////////////////////////////////////////////////////////////////////////

  ============
  Error Module 
  ============

  Module for error reporting macros and functions. 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_ERROR_H
#define INC_ERROR_H

#include "globals.h"

/* Wrappers for frequently occurring calls to fprintf. */
#define print_to_console(error_message, ...)                \
  do { fprintf(stderr, error_message, ##__VA_ARGS__); }     \
  while(0) 

#define print_to_log(error_message, ...)                    \
  do { fprintf(log_file, error_message, ##__VA_ARGS__); }   \
  while(0)

extern FILE *log_file;
/* Compile time log file with name <file_name>.log */
void openLogFileC(string file_name);
/* Runtime log file with name gp2.log */
void openLogFileR(void);
void closeLogFile(void);

#endif /* INC_ERROR_H */

