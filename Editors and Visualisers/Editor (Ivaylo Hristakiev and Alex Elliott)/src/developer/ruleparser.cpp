/*!
 * \file
 */
#include "ruleparser.hpp"
#include <QDebug>
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
//#define PARSER_TRACE
int parse_target = 0; 
extern "C" { int yyparse (); int yydebug; } 	// yyparse is an external C (not C++) function, yydebug is defined by the parser

namespace Developer {

// returns true if the passed file is a valid rule
static bool validateRule(const QString &rulePath)
{
    QFileInfo f(rulePath);
    if (!f.exists())
    {
        qDebug() << "    Rule file does not exist: " << rulePath;
        return false;
    }

    //int fileHandle = ruleFP->handle();
    // assumes the QFile is already open
    yyin = fopen( rulePath.toStdString().c_str()  , "r");

    #ifdef PARSER_TRACE
        yydebug = 1; /* Bison outputs a trace of its parse to stderr. */
    #endif
    parse_target = GP_RULE;

    /* yyparse sets the global flag syntax_error to true if any syntax errors
    * exist in the program. Some syntax errors are cleanly handled by the parser,
    * resulting in a valid AST. Hence, if syntax errors are encountered, semantic
    * analysis can still be performed. */
    int result = yyparse();

    if (syntax_error) qDebug() << "  Syntax errors were discovered by the parser.";

    return (result == 0);			// yyparse will (somehow) close yyin
}

rule_t parseRule(const QString &rulePath)
{
    rule_t ret;

    bool r = validateRule(rulePath);
    //bool r = false;

    if(!r)
    {
        qDebug() << "    Rule parsing failed." ;
				return ret;
    }
    /*else if(iter != end)
    {
        std::cout << "    Parsing ended before the end of the provided string."
                  << std::endl;
        std::cout << "    Remaining string contents: " << std::string(iter, end)
                  << std::endl;
    }*/

    Developer::reverseRuleAST(gp_rule);
    ret = Developer::translateRule(gp_rule);
//    qDebug() << "    ruleparser.cpp: " <<  QString::fromStdString(ret.condition);

//    int size = 0;
//    for (std::vector<param_t>::const_iterator it = ret.parameters.begin(); it != ret.parameters.end(); ++it)
//    {
//        param_t v = *it;
////        if (v.type != "list")
////            continue;
//        qDebug() << "    ruleparser.cpp: " << QString(v.type.c_str());
//        for (std::vector<std::string>::const_iterator itt = v.variables.begin(); itt != v.variables.end(); ++itt)
//        {
//            qDebug() << "    ruleparser.cpp:" << QString(itt->c_str());
//            size++;
//        }
//    }
//    qDebug() << "    ruleparser.cpp: The extracted rule_t structure has" << size << "variables";

    return ret;
}

}
