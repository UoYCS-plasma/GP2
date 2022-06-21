/* Copyright 2015-2017 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software:
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>. */

#include "genCondition.h"

/* For each predicate in the condition, generate a boolean value 'bx', where x
 * is the ID of the predicate. The variables are initialised in such a way that
 * the condition always evaluates to true, so that the condition isn't erroneously
 * falsified when one of these variables is modified by the evaluation of a
 * predicate. */
void generateConditionVariables(Condition *condition)
{
   static int bool_count = 0;
   switch(condition->type)
   {
      /* Booleans representing positive predicates are initialised with true. */
      case 'e':
           PTF("bool b%d = true;\n", bool_count++);
           break;

      /* Booleans representing 'not' predicates are initialised with false. */
      case 'n':
           PTF("bool b%d = false;\n", bool_count++);
           generateConditionVariables(condition->neg_condition);
           break;

      case 'a':
      case 'o':
           generateConditionVariables(condition->left_condition);
           generateConditionVariables(condition->right_condition);
           break;

      default:
           print_to_log("Error (generateConditionVariables): Unexpected condition "
                        "type '%c'.\n", condition->type);
           break;
   }
}

/* Generates a boolean expression over the variables generated by the above function.
 * The tree structure of the condition is used to print the correct expression. */
void generateConditionEvaluator(Condition *condition, bool nested)
{
   static int bool_count = 0;
   if(!nested)
   {
      PTF("static bool evaluateCondition(void)\n");
      PTF("{\n");
      PTFI("return (", 3);
   }
   switch(condition->type)
   {
      case 'e':
           PTF("b%d", bool_count++);
           break;

      case 'n':
           PTF("!b%d", bool_count++);
           break;

      case 'a':
           if(nested) PTF("(");
           generateConditionEvaluator(condition->left_condition, true);
           PTF(" && ");
           generateConditionEvaluator(condition->right_condition, true);
           if(nested) PTF(")");
           break;

      case 'o':
           if(nested) PTF("(");
           generateConditionEvaluator(condition->left_condition, true);
           PTF(" || ");
           generateConditionEvaluator(condition->right_condition, true);
           if(nested) PTF(")");
           break;

      default:
           print_to_log("Error (generateConditionExpression): Unexpected condition "
                        "type '%c'.\n", condition->type);
           break;
   }
   if(!nested)
   {
      PTF(");\n");
      PTF("}\n\n");
   }
}

static bool labelIsIntegerExpression(RuleLabel label)
{
   if(label.length != 1) return false;
   switch(label.list->first->atom->type)
   {
      case INTEGER_CONSTANT:
      case LENGTH:
      case INDEGREE:
      case OUTDEGREE:
      case NEG:
      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
           return true;

      case VARIABLE:
           if(label.list->first->atom->variable.type == INTEGER_VAR) return true;
           else return false;

      case STRING_CONSTANT:
      case CONCAT:
           return false;

      default: return false;
   }
   return false;
}

/* Writes a function that evaluates a predicate. The generated function checks
 * if all appropriate nodes and variables are instantiated. If so, it sets the
 * appropriate runtime boolean value to the result of the predicate's evalution
 * and returns true. Otherwise, it returns false. */
static void generatePredicateCode(Rule *rule, Predicate *predicate)
{
   PTF("static void evaluatePredicate%d(Morphism *morphism)\n", predicate->bool_id);
   PTF("{\n");
   int index;
   /* Generate code for any nodes that participate in this predicate. */
   for(index = 0; index < rule->lhs->node_index; index++)
   {
      RuleNode *node = getRuleNode(rule->lhs, index);
      if(node->predicates == NULL) continue;
      int p;
      for(p = 0; p < node->predicate_count; p++)
      {
         if(node->predicates[p] == predicate)
         {
            PTFI("Node *n%d = lookupNode(morphism, %d);\n", 3, index, index);
            PTFI("/* If the node is not yet matched by the morphism, return. */\n", 3);
            PTFI("if(n%d == NULL) return;\n\n", 3, index);
            break;
         }
      }
   }
   /* Generate code for any variables that participate in this predicate. */
   for(index = 0; index < rule->variables; index++)
   {
      Variable variable = rule->variable_list[index];
      if(variable.predicates == NULL) continue;
      int p;
      for(p = 0; p < variable.predicate_count; p++)
      {
         if(variable.predicates[p] == predicate)
         {
            PTFI("Assignment assignment_%d = getAssignment(morphism, %d);\n",
                 3, index, index);
            PTFI("/* If the variable is not yet assigned, return. */\n", 3);
            PTFI("if(assignment_%d.type == 'n') return;\n", 3, index);
            switch(variable.type)
            {
               case INTEGER_VAR:
                    PTFI("int var_%d = getIntegerValue(morphism, %d);\n\n", 3,
                         index, index);
                    break;

               case CHARACTER_VAR:
               case STRING_VAR:
                    PTFI("string var_%d = getStringValue(morphism, %d);\n\n", 3,
                         index, index);
                    break;

               case ATOM_VAR:
               case LIST_VAR:
                    PTFI("Assignment var_%d = assignment_%d;\n", 3, index, index);
                    break;

               default:
                    print_to_log("Error (generateVariableCode): Unexpected type %d\n",
                                 variable.type);
                    break;
            }
            break;
         }
      }
   }
   int list_count = 0;
   switch(predicate->type)
   {
      case INT_CHECK:
           PTFI("if(assignment_%d.type == 'i') b%d = true;\n", 3,
                predicate->variable_id, predicate->bool_id);
           PTFI("else b%d = false;\n", 3, predicate->bool_id);
           break;

      case CHAR_CHECK:
           PTFI("if(assignment_%d.type == 's' &&\n", 3, predicate->variable_id);
           PTFI("strlen(assignment_%d.string) == 1)\n", 6, predicate->variable_id);
           PTFI("b%d = true;\n", 6, predicate->bool_id);
           PTFI("else b%d = false;\n", 3, predicate->bool_id);
           break;

      case STRING_CHECK:
           PTFI("if(assignment_%d.type == 's') b%d = true;\n",
                3, predicate->variable_id, predicate->bool_id);
           PTFI("else b%d = false;\n", 3, predicate->bool_id);
           break;

      case ATOM_CHECK:
           PTFI("if(assignment_%d.type != 'l') b%d = true;\n",
                3, predicate->variable_id, predicate->bool_id);
           PTFI("else b%d = false;\n", 3, predicate->bool_id);
           break;

      case EDGE_PRED:
      {
           int source = predicate->edge_pred.source;
           int target = predicate->edge_pred.target;
           PTFI("bool edge_found = false;\n", 3);
           PTFI("EdgeList *elist = NULL;\n", 3);
           //PTFI("for(counter = 0; counter < source->out_edges.size + 2; counter++)\n", 3);
           PTFI("for(Edge *edge; (edge = yieldNextOutEdge(host, n%d, &elist)) != NULL;)\n", 3, source);
           PTFI("{\n", 3);
           PTFI("if(edge != NULL && edgeTarget(edge) == n%d)\n", 6, target);
           if(predicate->edge_pred.label.length >= 0)
           {
              PTFI("{\n", 6);
              PTFI("HostLabel label;\n", 9);
              /* Create runtime variables for each variable in the label. */
              if(predicate->edge_pred.label.length > 0)
              {
                 RuleListItem *item = predicate->edge_pred.label.list->first;
                 int count;
                 for(count = 0; count < predicate->edge_pred.label.length; count++)
                 {
                    if(item->atom->type == VARIABLE)
                    {
                       /* generateVariableCode prints with indent 3. Indent of 9 is required. */
                       PTF("      ");
                       generateVariableCode(item->atom->variable.id, item->atom->variable.type);
                    }
                    item = item->next;
                 }
              }
              bool label_any_marked = false;
              if (predicate->edge_pred.label.mark == ANY) {
                label_any_marked = true;
                predicate->edge_pred.label.mark = GREY;
              }
              generateLabelEvaluationCode(predicate->edge_pred.label, false, list_count++, 1, 9);
              if (label_any_marked)
              {
                 PTFI("if(equalHostLabelsModMarks(label, edge->label))\n", 9);
                 predicate->edge_pred.label.mark = ANY;
              }
              else
              {
                 PTFI("if(equalHostLabels(label, edge->label))\n", 9);
              }
              PTFI("{\n", 9);
              PTFI("b%d = true;\n", 12, predicate->bool_id);
              PTFI("edge_found = true;\n", 12);
              if(!minimal_gc) PTFI("removeHostList(label.list);\n", 12);
              PTFI("break;\n", 12);
              PTFI("}\n", 9);
              if(!minimal_gc) PTFI("removeHostList(label.list);\n", 9);
              PTFI("}\n", 6);
           }
           else
           {
              PTFI("{\n", 6);
              PTFI("b%d = true;\n", 9, predicate->bool_id);
              PTFI("edge_found = true;\n", 9);
              PTFI("break;\n", 9);
              PTFI("}\n", 6);
           }
           PTFI("}\n", 3);
           PTFI("if(!edge_found) b%d = false;\n", 3, predicate->bool_id);
           break;
      }
      case EQUAL:
      case NOT_EQUAL:
      {
           RuleLabel left_label = predicate->list_comp.left_label;
           RuleLabel right_label = predicate->list_comp.right_label;
           /* If the lists are integer constants, generate integer expressions
            * and compare them. Otherwise, generate full lists for comparison. */
           if(labelIsIntegerExpression(left_label) && labelIsIntegerExpression(right_label))
           {
              PTFI("if(", 3);
              generateIntExpression(left_label.list->first->atom, 1, false);
              if(predicate->type == EQUAL) PTF(" == ");
              if(predicate->type == NOT_EQUAL) PTF(" != ");
              generateIntExpression(right_label.list->first->atom, 1, false);
              PTF(") b%d = true;\n", predicate->bool_id);
              PTFI("else b%d = false;\n", 3, predicate->bool_id);
           }
           else
           {
              generateLabelEvaluationCode(left_label, false, list_count++, 2, 3);
              generateLabelEvaluationCode(right_label, false, list_count++, 3, 3);
              PTFI("if(", 3);
              if(predicate->type == NOT_EQUAL) PTF("!");
              PTF("equalHostLists(array%d, array%d, list_length%d, list_length%d)) "
                  "b%d = true;\n", list_count - 2, list_count - 1, list_count - 2,
                  list_count - 1, predicate->bool_id);
              PTFI("else b%d = false;\n", 3, predicate->bool_id);
           }
           break;
      }
      case GREATER:
      case GREATER_EQUAL:
      case LESS:
      case LESS_EQUAL:
           PTFI("if(", 3);
           generateIntExpression(predicate->atom_comp.left_atom, 1, false);
           if(predicate->type == GREATER) PTF(" > ");
           if(predicate->type == GREATER_EQUAL) PTF(" >= ");
           if(predicate->type == LESS) PTF(" < ");
           if(predicate->type == LESS_EQUAL) PTF(" <= ");
           generateIntExpression(predicate->atom_comp.right_atom, 1, false);
           PTF(") b%d = true;\n", predicate->bool_id);
           PTFI("else b%d = false;\n", 3, predicate->bool_id);
           break;

      default:
           print_to_log("Error (generatePredicateCode): Unexpected type %d.\n",
                        predicate->type);
           break;
   }
   PTF("}\n\n");
}

void generatePredicateEvaluators(Rule *rule, Condition *condition)
{
   switch(condition->type)
   {
      case 'e':
           generatePredicateCode(rule, condition->predicate);
           break;

      case 'n':
           generatePredicateEvaluators(rule, condition->neg_condition);
           break;

      case 'a':
      case 'o':
           generatePredicateEvaluators(rule, condition->left_condition);
           generatePredicateEvaluators(rule, condition->right_condition);
           break;

      default:
           print_to_log("Error (generatePredicateEvaluators): Unexpected condition "
                        "type '%c'.\n", condition->type);
           break;
   }
}