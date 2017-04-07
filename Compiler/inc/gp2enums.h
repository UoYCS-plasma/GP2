/* ///////////////////////////////////////////////////////////////////////////

  Copyright 2015-2016 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>.

/////////////////////////////////////////////////////////////////////////// */

#ifndef GP2ENUMS_H
#define GP2ENUMS_H

/* GP 2's variable types. */
typedef enum {INTEGER_VAR = 0, CHARACTER_VAR, STRING_VAR, ATOM_VAR, LIST_VAR} GPType;

typedef enum {NONE = 0, RED, GREEN, BLUE, GREY, DASHED, ANY} MarkType; 

typedef enum {INT_CHECK = 0, CHAR_CHECK, STRING_CHECK, ATOM_CHECK, EDGE_PRED,
              EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, 
              BOOL_NOT, BOOL_OR, BOOL_AND } ConditionType;

typedef enum {INTEGER_CONSTANT = 0, STRING_CONSTANT, VARIABLE, LENGTH, INDEGREE,
              OUTDEGREE, NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} AtomType;

#endif /* GP2ENUMS_H */
