/* ///////////////////////////////////////////////////////////////////////////

  ============
  Error Module 
  ============

  Module for error reporting macros and functions. 

/////////////////////////////////////////////////////////////////////////// */
#ifndef INC_ERROR_H
#define INC_ERROR_H

#ifdef __cplusplus
extern "C" {
#else
#endif




#include "globals.h"
//#include "string.h"

/* Wrappers for frequently occurring calls to fprintf. */
#define print_error(error_message, ...)                   \
  do {  fprintf(stderr, error_message, ##__VA_ARGS__);  } 	\
  while(0) 

#define print_to_console(error_message, ...)                \
  do { fprintf(stderr, error_message, ##__VA_ARGS__); }     \
  while(0) 

#define print_to_log(error_message, ...)                    \
  do { fprintf(stderr, error_message, ##__VA_ARGS__); }   \
  while(0)

#define print_trace(trace, ...)                       \
  do { fprintf(stderr, trace, ##__VA_ARGS__); }   \
  while(0)
/*
extern FILE *log_file;
 Open the log file gp2.log for writing. 
void openLogFile(string log_file_name);
void closeLogFile(void);

extern FILE *trace_file;
void openTraceFile(string trace_file_name);
void closeTraceFile(void);*/

#ifdef __cplusplus
}
#endif
#endif /* INC_ERROR_H */
