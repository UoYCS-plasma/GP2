/*!
 * \file
 */
#ifndef PARSERTYPES_HPP
#define PARSERTYPES_HPP

//#include <QDebug>

#ifndef Q_MOC_RUN
//#include <boost/variant/recursive_variant.hpp>
//#include <boost/fusion/include/adapt_struct.hpp>
//#include <boost/optional.hpp>
#include <boost/variant.hpp>
#endif // Q_MOC_RUN

#include <vector>
#include <string>

namespace Developer {

typedef boost::variant< int, std::string > atom_t;


/*!
 * The label_t represents a GP label; implemented by the 'Label' grammar productions
 */
struct label_t
{
    std::vector<atom_t> values;
		//! The node's (optional) mark - red, green, blue, grey, dashed, any (for rules only)
    //boost::optional<bool> marked;
	  std::string mark;
};

/*!
 * \brief The node_t struct is a POD datatype for representing a node; implemented by the 'Node' grammar productions
 */
struct node_t
{
    //! The node's identifier
    std::string id;
    //! The node's label
    label_t label;
    //! The node's x coordinate within the canvas
    double xPos;
    //! The node's y coordinate within the canvas
    double yPos;
	  //! The node can be a root node
	  bool isRoot;
};

/*!
 * \brief The edge_t struct is a POD datatype for representing an edge
 * Implemented by the 'Edge' grammar productions
 */
struct edge_t
{
    //! The edge's identifier
    std::string id;
    //! The identifier of the "from" node
    std::string from;
    //! The identifier of the "to" node
    std::string to;
    //! The edge's label
    label_t label;
	  //! The edge can be a bidirectional edge
	  bool isBidirectional;
};

/*!
 * \brief The graph_t struct is a POD datatype for representing a graph
 * Implemented by the 'Graph' grammar productions
 */
struct graph_t
{
    //! The x dimension for the canvas in pixels
    double canvasX;
    //! The y dimension for the canvas in pixels
    double canvasY;
    //! A vector containing the graph's nodes
    std::vector<node_t> nodes;
    //! A vector containing the graph's edges
    std::vector<edge_t> edges;
};

/*!
 * \brief The param_t struct is a POD datatype for representing a typed
 *  parameter
 *
 * Strictly speaking this can be a comma delimited list of variables with a
 * single shared type
 * Implemented by the 'VarList' grammar productions
 */
struct param_t
{
    //! The parameter's identifier
    std::vector<std::string> variables;
    //! The paramater's datatype (list, atom, int, string)
    std::string type;
};

/*!
 * \brief The interface_t struct is a POD datatype for representing an interface
 *  between the LHS and RHS graphs
 * Implemented by the 'Inter' grammar productions
 */
struct interface_t
{
    //! The ID of the LHS node
    //std::string lhsId;
    //! The ID of the RHS node
    //std::string rhsId;
    //! The IDs of interface elements
    std::vector<std::string> elements;
};

/*!
 * \brief The rule_t struct is a POD datatype for representing a rule
 * Implemented by the 'RuleDecl' grammar productions
 */
struct rule_t
{
    //! The contents of an initial // comment
    std::string documentation;
    //! The rule's identifier
    std::string id;
    //! A vector containing the rule's parameters
    std::vector<param_t> parameters;
    //! The rule's LHS graph
    graph_t lhs;
    //! The rule's RHS graph
    graph_t rhs;
    //! The interface between the LHS and RHS
    interface_t interface;
    //! The rule's (optional) condition
    std::string condition;
};


}

#endif // PARSERTYPES_HPP
