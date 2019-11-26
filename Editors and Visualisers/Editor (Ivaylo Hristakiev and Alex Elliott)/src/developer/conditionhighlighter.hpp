/*!
 * \file
 */
#ifndef CONDITIONHIGHLIGHTER_HPP
#define CONDITIONHIGHLIGHTER_HPP

#include <QSyntaxHighlighter>
#include "token.hpp"

namespace Developer {

class ConditionHighlighter : public QSyntaxHighlighter
{
    Q_OBJECT
public:
    explicit ConditionHighlighter(QTextDocument *parent = 0);

    /*!
     * \brief Gets the format associated with the provided type
     * \param type  The type of the token whose format we are retrieving. This
     *  will be a value from the Lexemes enum
     * \return The QTextCharFormat associated with this lexeme
     */
    QTextCharFormat format(int type) const;

public slots:
    /*!
     * \brief Set the sequence of tokens which correspond with this program
     * \param tokens    The sequence of tokens
     */
    void setTokens(QVector<Token *> tokens);
    /*!
     * \brief A wrapper function which complies with the standard
     *  QSyntaxHighlighter interface
     * \param text  The text contents of the current block being highlighted
     */
    void highlightBlock(const QString &text);

private:
    QVector<Token *> _tokens;
};

}

#endif // CONDITIONHIGHLIGHTER_HPP
