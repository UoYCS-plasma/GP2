/*!
 * \file
 */
#include "dotparser.hpp"

#include <QPointF>
#include <QDebug>

namespace Developer {

DotParser::DotParser(const QString &dotString)
    : _contents(dotString)
    , _pos(0)
    , _document(0)
    , _cursor(0)
{
    if(!dotString.isEmpty())
        parse();
}

DotParser::~DotParser()
{
    if(_cursor != 0)
        delete _cursor;
    if(_document != 0)
        delete _document;
}

bool DotParser::parse(const QString &dotString)
{
    if(!dotString.isEmpty())
        _contents = dotString;

    if(_cursor != 0)
        delete _cursor;
    if(_document != 0)
        delete _document;

    _document = new QTextDocument(_contents);
    _cursor = new QTextCursor(_document);

    _graph.canvasX = 0.0;
    _graph.canvasY = 0.0;
    _graph.nodes.clear();
    _graph.edges.clear();
    _pos = 0;
    _nodes.clear();
    _edges.clear();
    _idCounter = 1;

    return parseGraph();
}

bool DotParser::parseGraph()
{
    while(_pos < _contents.length())
    {
        if(consumeWhitespace() || consumeComments())
            continue;

        QRegExp rx = pattern(GraphOpen);
        if(rx.indexIn(_contents,_pos) == _pos)
        {
            _pos += rx.matchedLength();

            rx = pattern(GraphClose);

            while(_pos < _contents.length())
            {
                if(consumeWhitespace() || consumeComments())
                    continue;

                if(rx.indexIn(_contents,_pos) == _pos)
                    return true;

                if(!parseItem())
                    consumeError();
            }
        }
        else
            consumeError();
    }

    return false;
}

bool DotParser::parseItem()
{
    QRegExp rx = QRegExp(pattern(Identifier).pattern() + "|"
                         + pattern(QuotedString).pattern());
    int statementStart = _pos;
    bool statement = true;
    if(rx.indexIn(_contents, _pos) == _pos)
    {
        QString id;
        QString label;
        if(pattern(QuotedString).indexIn(_contents,_pos) == _pos)
        {
            // Check if this quoted string corresponds to an existing label
            label = rx.cap(0);
            //label.remove("\"");
            for(size_t i = 0; i < _graph.nodes.size(); ++i)
            {
                node_t n = _graph.nodes.at(i);

                // First of all check if we have a value to test against, it
                // must have exactly one, which should be a string value
                if(n.label.values.size() != 1)
                    continue;

                // It does have one value, extract it
                atom_t atom = n.label.values.at(0);

                std::string *str = boost::get<std::string>(&atom);
                if(!str)
                    continue;

                if(label == str->c_str())
                {
                    id = n.id.c_str();
                    break;
                }
            }

            if(id.isEmpty())
                id = label;
        }
        else
            id = rx.cap(0);

        _pos += rx.matchedLength();
        if(id == "graph")
        {
            // Parse graph properties
            while(consumeWhitespace() || consumeComments());

            QMap<QString, QVariant> attributes = parseAttributes();
            if(attributes.contains("bb"))
            {
                QString canvas = attributes["bb"].toString();
                QStringList rect = canvas.split(",");
                if(rect.length() < 4)
                {
                    qDebug() << "Could not parse bb value: " << canvas;
                    qDebug() << "Expected \\d+(,\\d+){3}";
                }

                _graph.canvasX = QVariant(rect.at(2)).toDouble();
                _graph.canvasY = QVariant(rect.at(3)).toDouble();
            }
        }
        else if(id == "subgraph" || id == "node" || id == "edge")
        {
            // Ignored properties
            while(consumeWhitespace() || consumeComments());
            parseAttributes();
        }
        else
        {
            // It's a regular ID, check if this is a node or an edge by looking
            // for an edge operator, then check for assignment as we ignore
            // those
            if(id.isEmpty())
            {
                id = QVariant(_idCounter).toString();
                while(_nodes.contains(id) || _edges.contains(id))
                {
                    ++_idCounter;
                    id = QVariant(_idCounter).toString();
                }
            }

            while(consumeWhitespace() || consumeComments());

            rx = pattern(EdgeOperator);
            if(rx.indexIn(_contents, _pos) == _pos)
            {
                // Edge
                _pos += rx.matchedLength();
                while(consumeWhitespace() || consumeComments());

                if(!_nodes.contains(id))
                {
                    if(label.isEmpty())
                        label = id;
                    label_t nodeLabel;
                    nodeLabel.values.push_back(label.toStdString());
                    node_t node;
                    node.id = id.toStdString();
                    node.label = nodeLabel;
                    _nodes << id;
                    _graph.nodes.push_back(node);
                }

                rx = pattern(Identifier);
                QString to;
                QString toLabel;
                if(rx.indexIn(_contents, _pos) == _pos)
                {
                    _pos += rx.matchedLength();
                    to = rx.cap(0);
                }
                else if(pattern(QuotedString).indexIn(_contents, _pos) == _pos)
                {
                    rx = pattern(QuotedString);
                    rx.indexIn(_contents,_pos);
                    _pos += rx.matchedLength();
                    toLabel = rx.cap(0);
                    toLabel.remove("\"");
                    for(size_t i = 0; i < _graph.nodes.size(); ++i)
                    {
                        node_t n = _graph.nodes.at(i);

                        // First of all check if we have a value to test against, it
                        // must have exactly one, which should be a string value
                        if(n.label.values.size() != 1)
                            continue;

                        // It does have one value, extract it
                        atom_t atom = n.label.values.at(0);

                        std::string *str = boost::get<std::string>(&atom);
                        if(!str)
                            continue;

                        if(toLabel == str->c_str())
                        {
                            to = n.id.c_str();
                            break;
                        }
                    }

                    if(to.isEmpty())
                    {
                        to = QVariant(_idCounter).toString();
                        while(_nodes.contains(to) || _edges.contains(to))
                        {
                            ++_idCounter;
                            to = QVariant(_idCounter).toString();
                        }
                    }
                }

                if(!to.isEmpty())
                {

                    if(!_nodes.contains(to))
                    {
                        if(toLabel.isEmpty())
                            toLabel = to;
                        label_t nodeLabel;
                        nodeLabel.values.push_back(toLabel.toStdString());
                        node_t node;
                        node.id = to.toStdString();
                        node.label = nodeLabel;
                        _nodes << to;
                        _graph.nodes.push_back(node);
                    }

                    while(consumeWhitespace() || consumeComments());

                    QMap<QString, QVariant> attributes = parseAttributes();
                    QString edgeId;
                    QString edgeLabel;
                    if(attributes.contains("id"))
                        edgeId = attributes["id"].toString();
                    else
                    {
                        edgeId = QVariant(_idCounter).toString();
                        while(_nodes.contains(edgeId) || _edges.contains(edgeId))
                        {
                            ++_idCounter;
                            edgeId = QVariant(_idCounter).toString();
                        }
                    }
                    if(attributes.contains("label"))
                        edgeLabel = QString("\"") + attributes["label"].toString() + "\"";

                    edge_t edge;

                    label_t edgeL;
                    if  (!edgeLabel.isEmpty() && (edgeLabel != QString("")) )
                            edgeL.values.push_back(edgeLabel.toStdString());

                    edge.id = edgeId.toStdString();
                    edge.label = edgeL;
                    edge.from = id.toStdString();
                    edge.to = to.toStdString();
                    _edges << edgeId;
                    _graph.edges.push_back(edge);
                }
            }
            else if(_contents.at(_pos) == QChar('='))
            {
                // Assignment
                statement = false;
                ++_pos;
                while(consumeWhitespace() || consumeComments());
                bool found = false;

                rx = pattern(Number);
                if(rx.indexIn(_contents,_pos) == _pos)
                {
                    _pos += rx.matchedLength();
                    found = true;
                }

                rx = pattern(Identifier);
                if(rx.indexIn(_contents,_pos) == _pos)
                {
                    _pos += rx.matchedLength();
                    found = true;
                }

                rx = pattern(QuotedString);
                if(rx.indexIn(_contents,_pos) == _pos)
                {
                    _pos += rx.matchedLength();
                    found = true;
                }

                if(!found)
                    consumeError();
            }
            else
            {
                // Node
                _nodes << id;
                while(consumeWhitespace() || consumeComments());

                QMap<QString, QVariant> attributes = parseAttributes();

                QPointF position;
                if(attributes.contains("label"))
                    label = QString("\"") + attributes["label"].toString() + "\"";
                if(attributes.contains("pos"))
                {
                    QString pos = attributes["pos"].toString();
                    QStringList coords = pos.split(",");
                    if(coords.length() > 1)
                    {
                        position.setX(QVariant(coords.at(0)).toDouble());
                        position.setY(QVariant(coords.at(1)).toDouble());
                    }
                }

                label_t nodeLabel;
                nodeLabel.values.push_back(label.toStdString());

                node_t node;
                node.id = id.toStdString();
                node.label = nodeLabel;
                node.xPos = position.x();
                node.yPos = position.y();
                _graph.nodes.push_back(node);
            }
        }

        while(consumeWhitespace() || consumeComments());

        if(statement || _contents.at(_pos) == QChar(';'))
        {
            rx = pattern(StatementSeparator);
            if(rx.indexIn(_contents,_pos) != _pos)
            {
                _cursor->setPosition(statementStart);
                qDebug() << QString("Dot Parser: Statement starting at %1:%2 "
                                    "was not closed.").arg(
                                QVariant(_cursor->blockNumber()).toString(),
                                QVariant(_cursor->positionInBlock()).toString()
                                ).toStdString().c_str();
                return false;
            }

            _pos += rx.matchedLength();
        }
        return true;
    }
    else
        return false;
}

QMap<QString,QVariant> DotParser::parseAttributes()
{
    QMap<QString,QVariant> attributes;

    QRegExp rx = pattern(AttributeListOpen);
    if(rx.indexIn(_contents,_pos) == _pos)
    {
        _pos += rx.matchedLength();
        while(_pos < _contents.length())
        {
            if(consumeWhitespace() || consumeComments())
                continue;

            rx = pattern(AttributeListClose);
            if(rx.indexIn(_contents,_pos) == _pos)
            {
                _pos += rx.matchedLength();
                return attributes;
            }

            rx = pattern(Identifier);
            if(rx.indexIn(_contents,_pos) == _pos)
            {
                QString name = rx.cap(0);
                _pos += rx.matchedLength();

                while(consumeWhitespace() || consumeComments());

                if(_contents.at(_pos) == QChar('='))
                {
                    ++_pos;
                    while(consumeWhitespace() || consumeComments());

                    rx = pattern(Number);
                    if(rx.indexIn(_contents,_pos) == _pos)
                    {
                        _pos += rx.matchedLength();
                        double number = QVariant(rx.cap(0)).toDouble();
                        attributes.insert(name, QVariant(number));
                        continue;
                    }

                    rx = pattern(QuotedString);
                    if(rx.indexIn(_contents,_pos) == _pos)
                    {
                        _pos += rx.matchedLength();
                        QString str = rx.cap(0);
                        str.remove("\"");
                        attributes.insert(name, str);
                        continue;
                    }

                    rx = pattern(Identifier);
                    if(rx.indexIn(_contents,_pos) == _pos)
                    {
                        _pos += rx.matchedLength();
                        attributes.insert(name, rx.cap(0));
                        continue;
                    }

                    qDebug() << "Dot Parser: bad attribute value given.";
                    qDebug() << name << "=" << _contents.mid(_pos, 20) + "...";
                    continue;
                }
                else
                {
                    qDebug() << "Attribute name followed by a non equals character "
                                << _contents.at(_pos);
                    continue;
                }
            }

            if(_contents.at(_pos) == QChar(','))
            {
                ++_pos;
                continue;
            }

            qDebug() << "Unexpected char " << _contents.at(_pos) << " at "
                        "position " << _pos;
            consumeError();
        }
    }

    return attributes;
}

bool DotParser::consumeWhitespace()
{
    QRegExp rx("\\s+");
    if(rx.indexIn(_contents,_pos) == _pos)
    {
        Q_ASSERT(rx.matchedLength() > 0);
        _pos += rx.matchedLength();
        return true;
    }
    else
        return false;
}

bool DotParser::consumeComments()
{
    // Check for a multi-linecomment
    QRegExp rx = pattern(CommentOpen);
    int matchPos = -1;
    if((matchPos = rx.indexIn(_contents,_pos)) == _pos)
    {
        // Comment found, now we need to check for an ending and if we can't
        // find one then we just mark a comment to the end of the program
        // and finish
        _pos += rx.matchedLength();
        rx = pattern(CommentClose);
        if((matchPos = rx.indexIn(_contents, _pos)) > 0)
        {
            // We found a closing token, set the end position correctly and
            // advance the position
            _pos = matchPos + rx.matchedLength();
            return true;
        }
        else
        {
            // There was no closing token, match to the end of the string
            _pos = _contents.length();
            return true;
        }
    }

    rx = pattern(SingleLineComment);
    if(rx.indexIn(_contents,_pos) == _pos)
    {
        _pos += rx.matchedLength();
        return true;
    }

    return false;
}

void DotParser::consumeError()
{
    QRegExp rx = pattern(Identifier);
    if(rx.indexIn(_contents,_pos) == _pos)
    {
        // This isn't a block, move along one char
        qDebug() << "Consume error taking identifier: " << rx.cap(0);
        _pos += rx.matchedLength();
        return;
    }

    rx = pattern(Number);
    if(rx.indexIn(_contents,_pos) == _pos)
    {
        // This isn't a block, move along one char
        qDebug() << "Consume error taking number: " << rx.cap(0);
        _pos += rx.matchedLength();
        return;
    }

    rx = pattern(QuotedString);
    if(rx.indexIn(_contents,_pos) == _pos)
    {
        // This isn't a block, move along one char
        qDebug() << "Consume error taking quoted string: " << rx.cap(0);
        _pos += rx.matchedLength();
        return;
    }


    qDebug() << "Consume error taking char: " << _contents.at(_pos);
    ++_pos;
}

graph_t DotParser::toGraph() const
{
    return _graph;
}

QRegExp DotParser::pattern(int type) const
{
    switch(type)
    {
    case SingleLineComment:
        return QRegExp("(//|#)[^\\n]\\n");
    case CommentOpen:
        return QRegExp("/\\*");
    case CommentClose:
        return QRegExp("\\*/");
    case Identifier:
        return QRegExp("[a-zA-Z0-9_]{1,63}\\b");
    case GraphOpen:
        return QRegExp("digraph\\s*([a-zA-Z0-9_]{,63})\\s*\\{");
    case GraphClose:
        return QRegExp("\\}");
    case EdgeOperator:
        return QRegExp("->");
    case AttributeListOpen:
        return QRegExp("\\[");
    case AttributeListClose:
        return QRegExp("\\]");
    case StatementSeparator:
        return QRegExp(";");
    case QuotedString:
        return QRegExp("\"[^\"]*\"");
    case Number:
        return QRegExp("-?(\\.\\d+|\\d+(\\.\\d*))\\b");
    default:
        qDebug() << "DotParser::pattern(): Unknown lexeme type: " << type;
        return QRegExp();
    }
}

}
