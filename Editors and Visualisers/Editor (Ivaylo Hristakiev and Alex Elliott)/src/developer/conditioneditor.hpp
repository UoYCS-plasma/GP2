/*!
 * \file
 */
#ifndef CONDITIONEDITOR_HPP
#define CONDITIONEDITOR_HPP

#include "codeeditor.hpp"
#include "conditiontokens.hpp"

namespace Developer {

class ConditionHighlighter;

class ConditionEditor : public CodeEditor
{
    Q_OBJECT

public:
    explicit ConditionEditor(QWidget *parent = 0);

    QRegExp pattern(int type) const;

    ConditionHighlighter *highlighter() const;

public slots:
    void parse();

protected:
    bool findMatch(ConditionLexemes *lexeme, int *matchLength,
                   ConditionLexemes hint = ConditionLexeme_Default);
    void handleLexeme(ConditionLexemes lexeme, int matchLength);

    void parseEdgeTest();

    bool consumeWhitespace();
    bool consumeComments();
    void consumeError(const QString &expecting = QString());

    /*!
     * \brief Produce a simple formatted text string when an invalid token is
     *  found
     *
     * This is just to keep the error messages looking consistent.
     *
     * \todo Convert position into line:column
     *
     * \param tokenFound    The unexpected token which was found
     * \param position      The position of the token
     * \return A formatted QString with the provided details
     */
    QString errorString(const QString &tokenFound, int position);

    /*!
     * \brief Catch mouse movement over the editor in order to overlay error
     *  descriptions in tooltips
     * \param e The mouse move event which has been captured
     */
    void mouseMoveEvent(QMouseEvent *e);

private:
    ConditionHighlighter *_highlighter;
    QString _condition;
    QString _cache;
    int _pos;
    QVector<Token *> _openParens;
    bool _wantsValue;
};

}

#endif // CONDITIONEDITOR_HPP
