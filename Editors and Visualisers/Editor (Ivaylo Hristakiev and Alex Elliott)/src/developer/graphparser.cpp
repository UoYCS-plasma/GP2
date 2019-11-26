/*!
 * \file
 */
#include "graphparser.hpp"
#include "dotparser.hpp"
/*
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_istream_iterator.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/phoenix_object.hpp>*/

#include <QDomDocument>
#include <QDebug>
#include <QStringList>
#include <QRegExp>
#include <QFile>
#include <QFileInfo>


extern "C" {
#include "translate/globals.h"
#include "translate/error.h"
#include "translate/ast.h"
#include "translate/parser.h"
#include "translate/pretty.h"
#include "translate/prettyGraph.h"
}

#include "translate/translate.hpp"

#define GP_GRAPH 2		
#define GP_RULE 3	
extern int parse_target;
// yyparse is an external C (not C++) function
extern "C" { int yyparse (); void yyrestart( FILE *new_file );}

namespace Developer {



// Internal helper function

// returns true if the passed file is a valid host graph
// assumes the QFile is already open
static bool validateHostGraph(const QString &graphPath)
{
    QFileInfo f(graphPath);
    if (!f.exists())
    {
        qDebug() << "    Graph file does not exist: " << graphPath;
        return false;
    }

    yyin = fopen( graphPath.toStdString().c_str() , "r");
    yyrestart(yyin);

    parse_target = GP_GRAPH;
    #ifdef PARSER_TRACE
        yydebug = 1; /* Bison outputs a trace of its parse to stderr. */
    #endif
    return (yyparse() == 0);
}

graph_t parseAlternativeGraph(const QString &graphPath)
{
    graph_t ret;

    bool r = validateHostGraph(graphPath);

    if(!r)
    {
        qDebug() << "    Graph parsing failed." ;
				return ret;
    }

    reverseGraphAST(ast_host_graph);

    ret = Developer::translateGraph(ast_host_graph);
    return ret;
}

graph_t parseDotGraph(const QString &graphString)
{
    DotParser dotParser(graphString);

    return dotParser.toGraph();
}

//graph_t parseGxlGraph(const QString &graphString)
//{
//    graph_t result;
//    result.canvasX = 0;
//    result.canvasY = 0;

//    QDomDocument doc("graph");
//    if(!doc.setContent(graphString))
//    {
//        qDebug() << "    Could not parse the input string, is it valid GXL?";
//        qDebug() << "    Input: " << graphString;
//        return result;
//    }

//    QDomNodeList nodes = doc.elementsByTagName("gxl");
//    if(nodes.count() < 1)
//    {
//        qDebug() << "    Parse Error: GXL input did not contain a <gxl> root node.";
//        qDebug() << "    Input: " << graphString;
//        return result;
//    }

//    QDomNode root = nodes.at(0);
//    nodes = root.childNodes();

//    for(int i = 0; i < nodes.count(); ++i)
//    {
//        QDomElement elem = nodes.at(i).toElement();
//        if(elem.tagName() == "graph")
//        {
//            // Now we're talking, we've got a graph. Check for canvas dimensions
//            if(elem.hasAttribute("canvasWidth"))
//                result.canvasX = elem.attribute("canvasWidth").toDouble();
//            if(elem.hasAttribute("canvasHeight"))
//                result.canvasY = elem.attribute("canvasHeight").toDouble();

//            nodes = nodes.at(i).childNodes();

//            // These are node IDs, which may be simple integers
//            QRegExp identifier("[a-zA-Z0-9_]{1,63}");
//            QRegExp remove("[^a-zA-Z0-9_]");
//            // We're not going to return to the parent loop, re-use i.
//            for(i = 0; i < nodes.count(); ++i)
//            {
//                elem = nodes.at(i).toElement();

//                // Handle nodes stored in this graph
//                if(elem.tagName() == "node")
//                {
//                    node_t node;
//                    node.xPos = 0;
//                    node.yPos = 0;

//                    // Start with compulsary attributes: id, label
//                    if(!elem.hasAttribute("id"))
//                    {
//                        qDebug() << "    Parse Error: <node> missing 'id' attribute.";
//                        continue;
//                    }
//                    else
//                    {
//                        QString id = elem.attribute("id");
//                        if(identifier.exactMatch(id))
//                            node.id = id.toStdString();
//                        else
//                        {
//                            qDebug() << "    Parse Warning: <node> id contains illegal characters. Stripping them.";
//                            qDebug() << "    Input: " << id;
//                            id.remove(remove);
//                            node.id = id.toStdString();
//                        }
//                    }

//                    if(!elem.hasAttribute("label"))
//                    {
//                        // This isn't an attribute, is it a child node?
//                        QDomNodeList children = nodes.at(i).childNodes();
//                        bool found = false;
//                        QString l;
//                        if(children.count() > 0)
//                        {
//                            for(int j = 0; j < children.count() && !found; ++j)
//                            {
//                                QDomElement e = children.at(j).toElement();
//                                if(e.tagName() == "label")
//                                {
//                                    l = e.text();
//                                    found = true;
//                                }
//                            }
//                        }

//                        if(!found)
//                        {
//                            qDebug() << "    Parse Warning: <node> missing 'label' attribute. Assuming label = id.";
//                            atom_t value = node.id.c_str();
//                            std::vector<atom_t> list;
//                            list.push_back(value);
//                            label_t label;
//                            label.values = list;

//                            node.label = label;
//                        }
//                        else
//                        {
//                            atom_t value = l.toStdString().c_str();
//                            std::vector<atom_t> list;
//                            list.push_back(value);
//                            label_t label;
//                            label.values = list;

//                            node.label = label;
//                        }
//                    }
//                    else
//                    {
//                        QString l = QVariant(elem.attribute("label")).toString();

//                        atom_t value = l.toStdString().c_str();
//                        std::vector<atom_t> list;
//                        list.push_back(value);
//                        label_t label;
//                        label.values = list;

//                        node.label = label;
//                    }

//                    // Then check for optional attributes: root, position
//                    if(elem.hasAttribute("root"))
//                    {
//                        if(QVariant(elem.attribute("root")).toBool())
//                        {
//                            node.id += "(R)";
//                        }
//                    }

//                    if(elem.hasAttribute("position"))
//                    {
//                        QStringList coords = elem.attribute("position").split(",");
//                        if(coords.size() < 2)
//                        {
//                            qDebug() << "    Parse Warning: <node> 'position' attribute does not contain a comma separated list of values, ignoring.";
//                        }
//                        else
//                        {
//                            node.xPos = coords.at(0).toDouble();
//                            node.yPos = coords.at(1).toDouble();
//                        }
//                    }

//                    result.nodes.push_back(node);
//                }
//                else if(elem.tagName() == "edge")
//                {
//                    edge_t edge;

//                    // Start with compulsary attributes: id, label
//                    if(!elem.hasAttribute("id"))
//                    {
//                        qDebug() << "    Parse Error: <edge> missing 'id' attribute.";
//                        continue;
//                    }
//                    else
//                    {
//                        QString id = elem.attribute("id");
//                        if(identifier.exactMatch(id))
//                            edge.id = id.toStdString();
//                        else
//                        {
//                            qDebug() << "    Parse Warning: <edge> id contains illegal characters. Stripping them.";
//                            qDebug() << "    Input: " << id;
//                            id.remove(remove);
//                            edge.id = id.toStdString();
//                        }
//                    }

//                    // Start with compulsary attributes: from, to
//                    if(!elem.hasAttribute("from"))
//                    {
//                        qDebug() << "    Parse Error: <edge> missing 'from' attribute";
//                        continue;
//                    }

//                    if(!elem.hasAttribute("to"))
//                    {
//                        qDebug() << "    Parse Error: <edge> missing 'to' attribute";
//                        continue;
//                    }

//                    QString fromId = elem.attribute("from");
//                    fromId.remove(remove);
//                    edge.from = fromId.toStdString();
//                    QString toId = elem.attribute("to");
//                    toId.remove(remove);
//                    edge.to = toId.toStdString();
//                    edge.to = elem.attribute("to").toStdString();

//                    // Then check for optional attributes: label
//                    if(elem.hasAttribute("label"))
//                    {
//                        label_t label;
//                        label.values.push_back(elem.attribute("label").toStdString());
//                        edge.label = label;
//                    }

//                    result.edges.push_back(edge);
//                }
//            }

//            return result;
//        }
//    }

//    return result;
//}

}
