/*!
 * \file
 */
#ifndef NODE_HPP
#define NODE_HPP

#include <QPointF>
#include <QObject>
#include <vector>

namespace Developer {


class Graph;
class Edge;

/*!
 * \brief The Node class represents a node within a GP graph object
 */
class Node : public QObject
{
    Q_OBJECT

public:
    Node(const QString &nodeId, const QString &nodeLabel = QString(), const QString &nodeMark = QString("none"), bool isRoot = false, bool isInterface = false,
         const QPointF &nodePos = QPointF(), Graph *parent = 0);

    QString id() const;
    QString label() const;
    QPointF pos() const;
    qreal xPos() const;
    qreal yPos() const;
    bool isRoot() const;
    bool isInterface() const;
    QString mark() const;
    bool isPhantomNode() const;

    std::vector<Edge *> edges() const;
    std::vector<Edge *> edgesFrom() const;
    std::vector<Edge *> edgesTo() const;

    bool hasEdgeIn() const;
    bool hasEdgeOut() const;

    Graph *parent() const;

    void setId(const QString &nodeId);
    void setLabel(const QString &nodeLabel);
    void setPos(const QPointF &nodePos);
    void setPos(qreal x, qreal y);
    void setIsRoot(bool root);
    void setIsInterface(bool isInterface);
    void setMark(const QString &mark);
    void setPhantom(bool phantom);


signals:
    void nodeChanged();
    void idChanged(QString id);
    void labelChanged(QString label);
    void isRootChanged(bool root);
    void isInterfaceChanged(bool root);
    void markChanged(QString mark);
    void isPhantomNodeChanged(bool phantom);

private:
    QString _id;
    QString _label;
    QString _mark;
    bool _isRoot;
    bool _isInterface;
    QPointF _pos;
    Graph *_parent;
    bool _phantom;
};

bool compareNodes (Node* node, Node* otherNode);
}

#endif // NODE_HPP
