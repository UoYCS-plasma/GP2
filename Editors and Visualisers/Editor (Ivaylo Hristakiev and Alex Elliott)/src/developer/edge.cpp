/*!
 * \file
 */
#include "edge.hpp"

namespace Developer {

Edge::Edge(const QString &edgeId, Node *fromNode, Node *toNode, const QString &edgeLabel, const QString &edgeMark, bool isBidirectional, Graph *parent)
    : _parent(parent)
    , _id(edgeId)
    , _from(fromNode)
    , _to(toNode)
    , _mark(edgeMark)
    , _isBidirectional(isBidirectional)
    , _label(edgeLabel)
    , _phantom(false)
{
}

QString Edge::id() const
{
    return _id;
}

Node *Edge::from() const
{
    return _from;
}

Node *Edge::to() const
{
    return _to;
}

QString Edge::label() const
{
    return _label;
}

Graph *Edge::parent() const
{
    return _parent;
}

bool Edge::isPhantomEdge() const
{
    return _phantom;
}

bool Edge::isBidirectional() const
{
    return _isBidirectional;
}

QString Edge::mark() const
{
    return _mark;
}

void Edge::setMark(const QString &mark)
{
    _mark = mark;
    emit edgeChanged();
    emit markChanged(mark);
}

void Edge::setId(const QString &edgeId)
{
    _id = edgeId;
    emit edgeChanged();
    emit idChanged(edgeId);
}

void Edge::setFrom(Node *fromNode)
{
    _from = fromNode;
    emit edgeChanged();
    emit fromChanged(fromNode);
}

void Edge::setTo(Node *toNode)
{
    _to = toNode;
    emit edgeChanged();
    emit toChanged(toNode);
}

void Edge::setLabel(const QString &edgeLabel)
{
    _label = edgeLabel;
    emit edgeChanged();
    emit labelChanged(edgeLabel);
}


void Edge::setPhantom(bool phantom)
{
    _phantom = phantom;
    emit edgeChanged();
    emit isPhantomEdgeChanged(phantom);
}

void Edge::setIsBidirectional(bool bidirectional)
{
    _isBidirectional = bidirectional;
    emit edgeChanged();
    emit isBidirectionalChanged(bidirectional);
}

}
