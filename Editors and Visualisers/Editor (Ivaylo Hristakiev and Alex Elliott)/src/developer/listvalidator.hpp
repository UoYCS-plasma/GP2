/*!
 * \file
 */
#ifndef LISTVALIDATOR_HPP
#define LISTVALIDATOR_HPP

#include <QValidator>

namespace Developer {

class ListValidator : public QValidator
{
    Q_OBJECT
private:
    explicit ListValidator(QObject *parent = 0);

    State validate(QString &input, int &pos) const;
};

}

#endif // LISTVALIDATOR_HPP
