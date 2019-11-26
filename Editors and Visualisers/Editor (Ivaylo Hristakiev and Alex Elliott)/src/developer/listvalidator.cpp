/*!
 * \file
 */
#include "listvalidator.hpp"

namespace Developer {

ListValidator::ListValidator(QObject *parent) :
    QValidator(parent)
{
}

ListValidator::State ListValidator::validate(QString &input, int &state) const
{
//    List l(input);
//    if(l.isClean())
//        return ListValidator::Acceptable;
//    else
        return ListValidator::Invalid;
}

}
