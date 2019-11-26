#include "prettyGraph.h"

extern unsigned int next_id;

void printDotHostGraph(GPGraph *const host_graph_ast, string file_name)
{
     int dot_length = strlen(file_name) + 5; 
     char dot_file_name[dot_length];
     strcpy(dot_file_name, file_name);
     strncat(dot_file_name, ".dot", 4);
     FILE *dot_file = fopen(dot_file_name, "w");
     
     if(dot_file == NULL) {
			perror(dot_file_name);
			exit(1);
     }	
     print_to_dot_file("digraph g { \n");

     /* Print the entry point of the AST. node1 will be the first 
      * node created by printGraph. */
     print_to_dot_file("node0[shape=plaintext,label=\"ROOT\"]\n");
     print_to_dot_file("node0->node1\n");

     next_id = 1;
     printASTGraph(host_graph_ast,dot_file);
     print_to_dot_file("}\n\n");
     fclose(dot_file);
}
