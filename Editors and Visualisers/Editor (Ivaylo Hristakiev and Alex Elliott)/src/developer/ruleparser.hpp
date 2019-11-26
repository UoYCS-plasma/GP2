/*!
 * \file
 */
#ifndef RULEPARSER_HPP
#define RULEPARSER_HPP

#include "parsertypes.hpp"
#include <QFile>

namespace Developer {

// Forward declaration of rule_t (parsertypes.hpp)
struct rule_t;

/*!
 * \brief parseRule takes in a string containing a saved rule and returns a
 *  rule_t datastructure with extracted data
 * \param rule  The rule to parse as a string. Not the path to a file.
 * \return A rule_t representing the rule passed in
 */
rule_t parseRule(const QString &rulePath);

}

#endif // RULEPARSER_HPP
