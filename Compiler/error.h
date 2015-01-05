/* ///////////////////////////////////////////////////////////////////////////

  ============
  Debug Module 
  ============

  Module for error reporting macros and functions. 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_ERROR_H
#define INC_ERROR_H

#include "globals.h"

/* Wrapper for frequently occurring calls to fprintf. */

#define print_to_console(error_message, ...)                \
  do { fprintf(stderr, error_message, ##__VA_ARGS__); }     \
  while(0) 

#define print_to_log(error_message, ...)                    \
  do { fprintf(log_file, error_message, ##__VA_ARGS__); }   \
  while(0)

extern FILE *log_file;
void openLogFile(string file_name);
void closeLogFile(void);

#endif /* INC_ERROR_H */

