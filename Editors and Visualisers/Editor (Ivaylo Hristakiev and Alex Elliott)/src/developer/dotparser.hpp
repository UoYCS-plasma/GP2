/*!
 * \file
 */
#ifndef DOTPARSER_HPP
#define DOTPARSER_HPP

#include <QRegExp>
#include <QStringList>
#include <QTextDocument>
#include <QTextCursor>
#include "parsertypes.hpp"

namespace Developer {

class DotParser
{
public:
    enum DotLexemes
    {
        SingleLineComment,
        CommentOpen,
        CommentClose,
        Identifier,
        GraphOpen,
        GraphClose,
        EdgeOperator,
        AttributeListOpen,
        AttributeListClose,
        StatementSeparator,
        QuotedString,
        Number
    };

    DotParser(const QString &dotString = QString());
    ~DotParser();

    bool parse(const QString &dotString = QString());
    bool parseGraph();
    bool parseItem();
    QMap<QString,QVariant> parseAttributes();

    bool consumeWhitespace();
    bool consumeComments();
    void consumeError();

    graph_t toGraph() const;

    QRegExp pattern(int type) const;

private:
    QString _contents;
    int _pos;
    graph_t _graph;
    QStringList _nodes;
    QStringList _edges;
    int _idCounter;
    QTextDocument *_document;
    QTextCursor *_cursor;
};

}

#endif // DOTPARSER_HPP
