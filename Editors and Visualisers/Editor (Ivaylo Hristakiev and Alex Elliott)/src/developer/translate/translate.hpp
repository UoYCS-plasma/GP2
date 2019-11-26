#ifndef TRANSLATE_HPP
#define TRANSLATE_HPP

#include "../parsertypes.hpp"
#include "ast.h"

namespace Developer {

atom_t 			translateAtom(GPAtom* atom);
label_t 		translateLabel(GPLabel* label);
node_t 			translateNode(GPNode* node);
edge_t 			translateEdge(GPEdge* edge);
graph_t 		translateGraph(GPGraph* graph);
interface_t translateInterface(List* interface);
std::vector<param_t> 		translateVariablesList(List* variables);
std::string translateCondition(GPCondition* condition, bool nested);
rule_t 			translateRule(GPRule* rule);

std::string ListToString(std::vector<atom_t> list);

void reverseRuleAST(GPRule *graph);

}

#endif //TRANSLATE_HPP
