/*!
 * \file
 */
#include "node.hpp"
#include "graph.hpp"

#include <QRegExp>

namespace Developer {



Node::Node(const QString &nodeId, const QString &nodeLabel, const QString &nodeMark, bool isRoot, bool isInterface, const QPointF &nodePos, Graph *parent)
    : QObject(parent)
    , _id(nodeId)
    , _label(nodeLabel)
    , _pos(nodePos)
    , _isRoot(isRoot)
    , _isInterface(isInterface)
    , _mark(nodeMark)
    , _parent(parent)
    , _phantom(false)
{
    if(_id.endsWith("(R)")) // ????????????????
    {
        _isRoot = true;
        _id.remove(QRegExp("\\((r|R)\\)$"));
    }
}


QString Node::id() const
{
    return _id;
}

QString Node::label() const
{
    return _label;
}

QPointF Node::pos() const
{
    return _pos;
}

qreal Node::xPos() const
{
    return _pos.x();
}

qreal Node::yPos() const
{
    return _pos.y();
}

bool Node::isRoot() const
{
    return _isRoot;
}

bool Node::isInterface() const
{
    return _isInterface;
}

QString Node::mark() const
{
    return _mark;
}

bool Node::isPhantomNode() const
{
    return _phantom;
}

std::vector<Edge *> Node::edges() const
{
    if(_parent == 0)
        return std::vector<Edge *>();

    return _parent->edges(id());
}

std::vector<Edge *> Node::edgesFrom() const
{
    if(_parent == 0)
        return std::vector<Edge *>();

    return _parent->edgesFrom(id());
}

std::vector<Edge *> Node::edgesTo() const
{
    if(_parent == 0)
        return std::vector<Edge *>();

    return _parent->edgesTo(id());
}

bool Node::hasEdgeOut() const
{
    if(_parent == 0)
        return false;

    return (_parent->hasEdgeFrom(id()));
}

bool Node::hasEdgeIn() const
{
    if(_parent == 0)
        return false;

    return (_parent->hasEdgeTo(id()));
}

Graph *Node::parent() const
{
    return _parent;
}

void Node::setId(const QString &nodeId)
{
    _id = nodeId;
    emit nodeChanged();
    emit idChanged(nodeId);
}

void Node::setLabel(const QString &nodeLabel)
{
    _label = nodeLabel;
    emit nodeChanged();
    emit labelChanged(nodeLabel);
}

void Node::setPos(const QPointF &nodePos)
{
    _pos = nodePos;
}

void Node::setPos(qreal x, qreal y)
{
    setPos(QPointF(x, y));
}

void Node::setIsRoot(bool root)
{
    _isRoot = root;
    emit nodeChanged();
    emit isRootChanged(root);
}

void Node::setIsInterface(bool isInterface)
{
    _isInterface = isInterface;
    //emit nodeChanged();   // Graph tracks if a node changes, but shouldnt in this case
    emit isInterfaceChanged(isInterface);
}

void Node::setMark(const QString &mark)
{
    _mark = mark;
    emit nodeChanged();
    emit markChanged(mark);
}

void Node::setPhantom(bool phantom)
{
    _phantom = phantom;
    emit nodeChanged();
    emit isPhantomNodeChanged(phantom);
}



bool compareNodes (Node* node, Node* otherNode)
{
    return (QString::compare(node->id(), otherNode->id()) < 0);
}


}
