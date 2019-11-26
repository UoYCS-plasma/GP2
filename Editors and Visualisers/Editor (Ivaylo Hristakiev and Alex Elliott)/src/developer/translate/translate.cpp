#include "translate.hpp"
#include "globals.h"
#include "error.h"
#include <sstream>	// for stringstream
#include <QDebug>

namespace Developer {

// Internal helper functions
std::string ListToString(std::vector<atom_t> list);
std::vector<atom_t> translateValues(List* list);
std::vector<node_t> translateNodes(List* nodes);
std::vector<edge_t> translateEdges(List* edges);
void reverseConditionAST(GPCondition *condition);
void reverseRuleAST(GPRule *rule);

std::stringstream extraVars;

std::vector<param_t> translateExtraVariables(std::vector<param_t> &declaredVars);

/*
 * Translates a GPAtom AST into an atom_t structure
 * Integer constants are represented by 'int' C values
 * String constants are represented by *quoted* C strings - e.g. "abc"
 * Variables are represented by their name and are C strings - e.g. a, b, c
 * Everything else is represented by a C string
 */
atom_t 			translateAtom(GPAtom* atom)
{
    atom_t result;
    if (!atom) return result;
    std::stringstream s;
    switch(atom->type)
    {
    case VARIABLE:
        result = std::string( atom->variable.name );
        extraVars << std::string( atom->variable.name ) + ","; // comma-delimited list
        break;

    case INTEGER_CONSTANT:
        result = atom->number;
        break;
    case STRING_CONSTANT:
        s << "\"" << atom->string << "\"";
        result = s.str();
        break;

    case NEG:
        s << "-" << translateAtom(atom->neg_exp);
        result = s.str();
        break;

    case LENGTH:
        s << "length(" << std::string( atom->variable.name ) << ")";
        result = s.str();
        break;

    case INDEGREE:
        s << "indeg(" << atom->node_id << ")";
        result = s.str();
        break;

    case OUTDEGREE:
        s << "outdeg(" << atom->node_id << ")";
        result = s.str();
        break;

    case ADD:
        s << translateAtom(atom->bin_op.left_exp) << "+" << translateAtom(atom->bin_op.right_exp);
        result = s.str();
        break;

    case SUBTRACT:
        s << translateAtom(atom->bin_op.left_exp) << "-" << translateAtom(atom->bin_op.right_exp);
        result = s.str();
        break;

    case MULTIPLY:
        s << translateAtom(atom->bin_op.left_exp) << "*" << translateAtom(atom->bin_op.right_exp);
        result = s.str();
        break;

    case DIVIDE:
        s << translateAtom(atom->bin_op.left_exp) << "/" << translateAtom(atom->bin_op.right_exp);
        result = s.str();
        break;

    case CONCAT:
        s << translateAtom(atom->bin_op.left_exp) << "." << translateAtom(atom->bin_op.right_exp);
        result = s.str();
        break;

    default:
        print_to_console("translate.cpp: Unknown conversion of GPAtom (%d)", atom->type); break;

    }

    return result;
}

/*  Translates a GPLabel AST into a label_t struct
 *  A label_t struct has a vector of atom values, which represents a GP2 list, and an optional mark
 *  The mark is represented by a C string
 *  The list is represented by a std::vector of atom_t values
 */
label_t 		translateLabel(GPLabel* label)
{
	label_t result;
	if (label==NULL) return result;
	switch (label->mark)
	{
		case NONE: result.mark = "";break;
  	case RED:  result.mark = "red";break;
		case GREEN: result.mark = "green";break;
		case BLUE: result.mark = "blue";break;
		case GREY: result.mark = "grey";break;
		case DASHED: result.mark = "dashed";break;
		case ANY:	  result.mark = "any";break;
		
		default: break;
	}
	result.values = translateValues(label->gp_list);

	return result;
}

/*  Translates a GPNode AST into a node_t struct
 *  A node_t struct has an id, a label_t label, canvas positions (double xPos, double yPos), and an optional isRoot flag
 *  The id is represented by a C string
 *  The label is represented by a label_t structure
 *  The root flag is a bool
 */
node_t 			translateNode(GPNode* node)
{	
	node_t result;
	if (node == NULL) return result;

	result.id = node->name;
	result.label = translateLabel(node->label);
	result.isRoot = node->root;

	return result;
}

/*  Translates a GPNode AST into a edge_t struct
 *  A edge_t struct has an id, a label_t label, source and target nodes, and an optional isBidirectional flag
 *  The id is represented by a C string
 *  The source and target nodes are represented by C strings holding the respective node ids
 *  The bidirectional flag is a bool
 */
edge_t 			translateEdge(GPEdge* edge)
{
	edge_t result;
	if (edge == NULL) return result;

	result.id = edge->name;
	result.label = translateLabel(edge->label);
	result.from = edge->source;
	result.to = edge->target;
	result.isBidirectional = edge->bidirectional;

	return result;
}




/* Translates a GPGraph AST into a graph_t struct
 * A graph_t has two collections of nodes and edges, and a canvas size specification
 */
graph_t 		translateGraph(GPGraph* graph)
{
	graph_t result;
	if (graph == NULL) return result;

	result.nodes = translateNodes(graph->nodes);
	result.edges = translateEdges(graph->edges);

	return result;
}

// Expects a comma-separated list of node ids
/* Translates a List of node(/edge) ids into an interface_t struct
 * An interface_t struct has a vector of elements, represented by their C string ids.
*/
interface_t translateInterface(List* interface)
{
	interface_t result;
	if (interface == NULL || (interface->type != INTERFACE_LIST)) return result;

	std::vector<std::string> elements;
	List* current_node = interface;

	while (current_node != NULL)
	{
		elements.push_back(current_node->node_id);
		current_node = current_node->next;
	}

	result.elements = elements;
	return result;
}

/*  Translates a list of variables of the same type into a param_t struct
 *  A param_t has a type and a collection of variables
 *  The type is represented by a C string
 *  The collection of variables is represented by a vector of strings
 */
param_t translateVariables(List* variables, std::string type)
{
	param_t result;
	if (variables == NULL) return result;
	result.type = type;

	std::vector<std::string> vvar;
	List* current_var = variables;
	while (current_var != NULL)
	{
		vvar.push_back(std::string(current_var->variable_name));
		current_var = current_var->next;
	}

	result.variables = vvar;
	return result;
}

/*  Translates a list of variables collections into a collection of param_t structs
 *  Each variables collection is a list of variables of a specific GP type (int, atom ..)
 */
std::vector<param_t> 		translateVariablesList(List* variables)
{
    std::vector<param_t> result;

    // If no variables in graph nodes/edges, then return
    if (extraVars.str().empty())
        return result;

    param_t validListVars; validListVars.type = "list";
    param_t validIntVars;   validIntVars.type = "int";
    param_t validCharVars;  validCharVars.type = "char";
    param_t validStringVars; validStringVars.type = "string";
    param_t validAtomVars;  validAtomVars.type = "atom";

    // Obtain the declared variables in the rule specification
    std::vector<param_t> declaredVars;

    List* current_list = variables;
    while (current_list != NULL)
    {
        switch (current_list->type)
        {
            case INT_DECLARATIONS:
                    declaredVars.push_back( translateVariables( current_list->variables , "int" ) );
                    break;
            case CHAR_DECLARATIONS:
                    declaredVars.push_back( translateVariables( current_list->variables , "char" ) );
                    break;
            case STRING_DECLARATIONS:
                    declaredVars.push_back( translateVariables( current_list->variables , "string" ) );
                    break;
            case ATOM_DECLARATIONS:
                    declaredVars.push_back( translateVariables( current_list->variables , "atom" ) );
                    break;
            case LIST_DECLARATIONS:
                    declaredVars.push_back( translateVariables( current_list->variables , "list" ) );
                    break;

            default: print_to_console("Unknown conversion of Variables list: %d", current_list->type); break;
        }
        current_list = current_list->next;
    }

    // Split the list of extra variables
    std::vector<std::string> extraVariables;
    std::istringstream iStream;     // :)
    iStream.str(extraVars.str().c_str());
    for (std::string var; std::getline( iStream, var, ',') ; ) // comma-delimited
    {
        extraVariables.push_back(var);
    }
    // sort + remove duplicates
    std::sort(extraVariables.begin(), extraVariables.end());
    extraVariables.erase( std::unique(extraVariables.begin(), extraVariables.end()), extraVariables.end());

    // Check which existing variable is declared - if not declared, then add it, otherwise keep with same type
    for (std::vector<std::string>::const_iterator it = extraVariables.begin() ; it != extraVariables.end()  ;++it)
    {
        std::string var = *it;
        bool declared = false;
        for (std::vector<param_t>::const_iterator itt = declaredVars.begin(); itt != declaredVars.end(); ++itt)
        {
            param_t varCollection = *itt;
            std::vector<std::string>::const_iterator position = std::find(varCollection.variables.begin(), varCollection.variables.end(), var);
            if (position != varCollection.variables.end())
            {
                // Existing variable was actually declared, check type and add to list of valid variables
                declared = true;
//                qDebug() << "    translate.cpp: Declared variable found:" << QString(var.c_str()) << QString(varCollection.type.c_str());

                if (varCollection.type == "list")
                    validListVars.variables.push_back(var);

                if (varCollection.type == "atom")
                    validAtomVars.variables.push_back(var);

                if (varCollection.type == "int")
                    validIntVars.variables.push_back(var);

                if (varCollection.type == "string")
                    validStringVars.variables.push_back(var);

                if (varCollection.type == "char")
                    validCharVars.variables.push_back(var);
            }
        }

        if (!declared)
            // Variable exists but was not declared, declare it as a list variable
        {
            qDebug() << "    translate.cpp: Undeclared variable:" << QString(var.c_str()) << " found, fixing.";
            validListVars.variables.push_back(var);
        }
    }

    // Now we are ready to populate the result - only add non-empty collections
    if (validListVars.variables.size() > 0)
        result.push_back(validListVars);

    if (validAtomVars.variables.size() > 0)
        result.push_back(validAtomVars);

    if (validIntVars.variables.size() > 0)
        result.push_back(validIntVars);

    if (validStringVars.variables.size() > 0)
        result.push_back(validStringVars);

    if (validCharVars.variables.size() > 0)
        result.push_back(validCharVars);

    return result;
}



/*  Translates a list of variables collections into a collection of param_t structs
 *  Each variables collection is a list of variables of a specific GP type (int, atom ..)
 */
//std::vector<param_t> 		translateVariablesList(List* variables)
//{
//	std::vector<param_t> result;
//    //if (variables == NULL) return result;

//	List* current_list = variables;

//    param_t listVarDeclarations;
//    listVarDeclarations.type = "list";
//    std::vector<std::string> existingListVars;

//	while (current_list != NULL)
//	{
//		switch (current_list->type)
//		{
//			case INT_DECLARATIONS:
//					result.push_back( translateVariables( current_list->variables , "int" ) );
//					break;
//			case CHAR_DECLARATIONS:
//					result.push_back( translateVariables( current_list->variables , "char" ) );
//					break;
//			case STRING_DECLARATIONS:
//					result.push_back( translateVariables( current_list->variables , "string" ) );
//					break;
//			case ATOM_DECLARATIONS:
//                    result.push_back( translateVariables( current_list->variables , "atom" ) );
//					break;
//			case LIST_DECLARATIONS:
//                    // Do nothing for list declarations at this point, need to check for extra variables first
//                    // Accumulate the declared list variables to a local collection
//                    {
//                        List* variables = current_list->variables;
//                        List* currentVar = variables;

//                        while (currentVar != NULL)
//                        {
//                            existingListVars.push_back(std::string(currentVar->variable_name));
//                            currentVar = currentVar->next;
//                        }

//                        qDebug () << "    translate.cpp: Found" << existingListVars.size() << "declared list variables";

//                        break;
//                    }

//            default: print_to_console("Unknown conversion of Variables list: %d", current_list->type); break;
//		}
//		current_list = current_list->next;
//	}

//    // Add the extra variables occuring in LHS/RHS labels
//    // Ignore those which are already declared
//    if (!extraVars.str().empty())
//    {
//        // Split the list of extra variables
//        std::vector<std::string> extraVariables;
//        std::istringstream iStream;     // :)
//        iStream.str(extraVars.str().c_str());
//        for (std::string var; std::getline( iStream, var, ',') ; ) // comma-delimited
//        {
//            extraVariables.push_back(var);
//        }

//        // sort + remove duplicates
//        std::sort(extraVariables.begin(), extraVariables.end());
//        extraVariables.erase( std::unique(extraVariables.begin(), extraVariables.end()), extraVariables.end());

//        // Iterate over the existing non-list variables
//        for (std::vector<param_t>::const_iterator it = result.begin();
//             it != result.end();
//             ++it)
//        {
//            param_t varDecl = *it;
//            for (std::vector<std::string>::iterator itt = varDecl.variables.begin();
//                 itt != varDecl.variables.end();
//                 ++itt)
//            {
//                std::string var = *itt;
//                std::vector<std::string>::iterator position = std::find(extraVariables.begin(), extraVariables.end(), var);
//                if (position != extraVariables.end())
//                {
//                    // Declaration of variable found, erase from extra list
//                    extraVariables.erase(position);
//                    qDebug() << "    translate.cpp: Declaration of variable" << QString(var.c_str()) << "found, type:" << QString(varDecl.type.c_str());
//                }
//                else
//                {
//                    // Variable was declared but not used anywhere in the actual graphs, remove ?
//                    qDebug() << "    translate.cpp: Found unused (but declared) variable" << QString(var.c_str()) << ", type:" << QString(varDecl.type.c_str());
//                    varDecl.variables.erase(position);
//                }
//            }
//        }

//        // Iterate over the declared list variables
//        for (std::vector<std::string>::iterator itt = existingListVars.begin();
//             itt != existingListVars.end();
//             ++itt)
//        {
//            std::string declaredVar = *itt;
//            // qDebug() << "    translate.cpp: Checking for duplication of variable" << QString(itt->c_str());

//            // Check if the declared variable is in the extraVariables list
//            std::vector<std::string>::iterator position = std::find(extraVariables.begin(), extraVariables.end() , declaredVar);
//            if (position != extraVariables.end())
//            {
//                // If found, modify the existng list of extra variables because they dont need to be declared
//                qDebug() << "    translate.cpp: Declaration of variable" << QString(declaredVar.c_str()) << "found, type: list";
//                extraVariables.erase(position);
//            }
//            else
//            {
//                // Variable was declared but not used anywhere in the actual graphs, remove ?
//                qDebug() << "    translate.cpp: Found unused (but declared) variable" << QString(declaredVar.c_str()) << ", type: list";

//            }
//        }

//        // Some variables were found in labels but were not declared
//        if (!extraVariables.empty())
//        {
//            bool foundListDeclarations = false;

//            qDebug() << "    translate.cpp: Size before changes:" << existingListVars.size();

//            for (std::vector<std::string>::const_iterator itt = extraVariables.begin();
//                 itt != extraVariables.end();
//                 ++itt)
//            {
//                std::string var = *itt;
//                qDebug() << "    translate.cpp: Declaring extra variable" << QString(var.c_str()) << "in the rule declaration.";
//                existingListVars.push_back(var);
//            }

//            qDebug() << "    translate.cpp: New size:" << existingListVars.size();
//        }
//    }

//    // If we found list variables that actually need to be declared (either preexisting or part of node/edge list expressions)
//    if (existingListVars.size() > 0)
//    {
//        listVarDeclarations.variables = existingListVars;
//        result.push_back(listVarDeclarations);
//    }

//	return result;
//}

/* Returns a C string representation of a GP list appearing in a rule's condition
 */
std::string translateConditionList(List* condition)
{
	return ListToString(translateValues(condition));
}

/* Translates a GPCondition AST into its underlying string representation
 * This is needed because rule_t structures have conditions as plain C strings
 */
std::string translateCondition(GPCondition* condition, bool nested)
{
	std::string result;
	if (condition == NULL) return result;
        std::stringstream s;
	switch(condition->type)
	{
        case INT_CHECK:     result = "int( "; result+= condition->var; result += " )" ; break;
        case CHAR_CHECK:    result = "char( "; result+= condition->var; result += " )" ; break;
        case STRING_CHECK: 	result = "string( "; result+= condition->var; result += " )" ; break;
        case ATOM_CHECK:    result = "atom( "; result+= condition->var; result += " )" ; break;

		case EQUAL:  				
            result = translateConditionList(condition->list_cmp.left_list);
            result+= "=";
            result+= translateConditionList(condition->list_cmp.right_list);
            break;
 
		case NOT_EQUAL: 
            result = translateConditionList(condition->list_cmp.left_list);
            result+= "!=";
            result+= translateConditionList(condition->list_cmp.right_list);
            break;

        case GREATER:
            s << translateAtom(condition->atom_cmp.left_exp);
            s << ">";
            s << translateAtom(condition->atom_cmp.right_exp);
            result = s.str();
            break;

        case GREATER_EQUAL:
            s << translateAtom(condition->atom_cmp.left_exp);
            s << ">=";
            s << translateAtom(condition->atom_cmp.right_exp);
            result = s.str();
            break;

        case LESS:
            s << translateAtom(condition->atom_cmp.left_exp);
            s << "<";
            s << translateAtom(condition->atom_cmp.right_exp);
            result = s.str();
            break;

        case LESS_EQUAL:
            s << translateAtom(condition->atom_cmp.left_exp);
            s << "<=";
            s << translateAtom(condition->atom_cmp.right_exp);
            result = s.str();
            break;

        case BOOL_NOT:
            result = "not ";
            result+= translateCondition(condition->not_exp, true);
            break;

		case BOOL_OR: 
            if (nested) result = "(";
            result+= translateCondition(condition->bin_exp.left_exp, true);
            result+= " or ";
            result+= translateCondition(condition->bin_exp.right_exp, true);
            if (nested) result+= ")";
            break;

		case BOOL_AND: 
            if (nested) result = "(";
            result+= translateCondition(condition->bin_exp.left_exp, true);
            result+= " and ";
            result+= translateCondition(condition->bin_exp.right_exp, true);
            if (nested) result+= ")";
            break;

		case EDGE_PRED: 
            result = "edge( ";
            result+= condition->edge_pred.source;
            result+= ", ";
            result+= condition->edge_pred.target;

            if (condition->edge_pred.label != NULL)
                result+= ", " + ListToString((translateLabel(condition->edge_pred.label)).values);

            result += " )";
            break;

		default: break;
	}
	return result;
}

/* Translates a GPRule AST into a rule_t struct 
 * A rule_t has documentation, an identifier, a list of variables, LHS and RHS graphs, an interface and a condition
 * The documentation is a C string
 * The rule id is a C string
 * The list of variables is a vector of param_t structs - each param_t is a collection of variables of the same type
 * The LHS and RHS are graph_t structs
 * The interface is an interface_t struct
 * The condition is a C string
 */
rule_t 			translateRule(GPRule* rule)
{
    // clear temporary coiolection of extra variables in GP atoms
    extraVars.str("");
    extraVars.clear(); // Clear state flags.

	rule_t result;
	result.documentation = std::string("");		// The GPRule AST doesn't store comments
    result.id = std::string(rule->name);
	result.lhs = translateGraph(rule->lhs);
    result.parameters = translateVariablesList(rule->variables);

	result.rhs = translateGraph(rule->rhs);
	result.interface = translateInterface(rule->interface);
    result.condition = translateCondition(rule->condition, false);


	return result;
}

/* 
 * The implementation of the helper functions
 */

// Expects a list of atom values
std::vector<atom_t> translateValues(List* list)
{
	std::vector<atom_t> result;

    // The empty list is represented by NULL
    if (list == NULL)
    {
        result.push_back("empty");
        return result;
    }

    if (list->type != GP_LIST)
        return result;


	result.reserve(getASTListLength(list));

	List *value = list;
	while (value != NULL)
	{
		result.push_back(translateAtom(value->atom));
		value = value->next;
	}

	return result;
}

std::string ListToString(std::vector<atom_t> list)
{
	std::stringstream s;

    for (std::vector<atom_t>::const_iterator it = list.begin(); it != list.end(); ++it)
	{
			s << *it;
			if (it != (list.end() - 1)) s << ":";
	}

	return s.str();
}
/*  Translates a List of nodes into a vector of node_t
 */
std::vector<node_t> translateNodes(List* nodes)
{
	std::vector<node_t> result;
	if (nodes == NULL || (nodes->type != NODE_LIST )) return result;

	result.reserve(getASTListLength(nodes));
	List* current_node = nodes;
	node_t n;
	while (current_node != NULL)
	{
		n = translateNode(current_node->node);
		result.push_back(n);

		current_node = current_node->next;
	}	

	return result;
}


/*  Translates a List of edges into a vector of edge_t
 */
std::vector<edge_t> translateEdges(List* edges)
{
	std::vector<edge_t> result;
	if (edges == NULL || (edges->type != EDGE_LIST )) return result;

	result.reserve(getASTListLength(edges));
	List* current_edge = edges;
	edge_t e;
	while (current_edge != NULL)
	{
		e = translateEdge(current_edge->edge);
		result.push_back(e);

		current_edge = current_edge->next;
	}	

	return result;
}

/* Extra AST functionality - the Bison parser grammar is left-recursive so all lists are in reverse order */

/* 
 * Reverses the lists pointed to by a GPCondition AST
 */
void reverseConditionAST(GPCondition *condition)
{
    switch(condition->type)
    {
    case INT_CHECK:
    case CHAR_CHECK: 
    case STRING_CHECK: 
    case ATOM_CHECK:
        break;

    case EDGE_PRED: 
        // struct GPLabel has a pointer (to a List) called gp_list
        if (condition->edge_pred.label)  // edge predicate has OPTIONAL label
            condition->edge_pred.label->gp_list = reverse(condition->edge_pred.label->gp_list);
        break;

    case EQUAL: 
    case NOT_EQUAL: 
        condition->list_cmp.left_list = reverse(condition->list_cmp.left_list);
        condition->list_cmp.right_list = reverse(condition->list_cmp.right_list);
        break;

    case BOOL_NOT: 
        reverseConditionAST(condition->not_exp);
        break;

    case BOOL_OR: 
    case BOOL_AND:
        reverseConditionAST(condition->bin_exp.left_exp);
        reverseConditionAST(condition->bin_exp.right_exp);
        break;

    case GREATER: 
    case GREATER_EQUAL: 
    case LESS: 
    case LESS_EQUAL: 

    default: break;
    }

}


/* 
 * Reverses a GPRule AST - it has a list of variables, a list of interface nodes/edges, a condition (which also may have lists in it) and ASTs for LHS and RHS graphs.
 */
void reverseRuleAST(GPRule *rule)
{
    if (rule == 0)
        return;

    if (rule->variables) rule->variables = reverse(rule->variables);
    if (rule->interface) rule->interface = reverse(rule->interface);

    if (rule->condition) reverseConditionAST(rule->condition);
    if (rule->lhs) reverseGraphAST(rule->lhs);
    if (rule->rhs) reverseGraphAST(rule->rhs);
}

}
