/*!
 * \file
 */
#ifndef NODEITEM_HPP
#define NODEITEM_HPP

#include "graphitem.hpp"

namespace Developer {

class Node;

class NodeItem : public GraphItem
{
    Q_OBJECT

public:
    enum NodeShapes
    {
        Circle,
        Ellipse,
        Rectangle,
        RoundedRectangle
    };

    explicit NodeItem(Node *node, QGraphicsItem *parent = 0);
    explicit NodeItem(const QString &nodeId,
                      const QString &nodeLabel = QString(),
                      const QString &nodeMark = QString("none"),
                      bool root = false,
                      QGraphicsItem *parent = 0);

    void recalculate();

    bool isRoot() const;
    bool isInterface() const;
    QString mark() const;
    Node *node() const;

    void setId(const QString &itemId);
    void setLabel(const QString &itemLabel);
    void setIsRoot(bool root);
    void setIsInterface(bool isInterface);
    void setMark(const QString &mark);

    void preserveNode();
    void deleteNode();

    QPainterPath shape() const;
    QList<QPointF> intersection(QLineF line) const;

    QPointF centerPos() const;
    QRectF boundingRect() const;
    void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
               QWidget *widget);

    QColor backgroundColor(const QStyleOptionGraphicsItem *option) const;
    QColor borderColor(const QStyleOptionGraphicsItem *option) const;

public slots:
    void addedEdge();

protected slots:
    void positionChanged();

signals:
    void edgeAdded();
    void shapeChanged();

protected:
    // Handle hover events
    void hoverEnterEvent(QGraphicsSceneHoverEvent *event);
    void hoverMoveEvent(QGraphicsSceneHoverEvent *event);
    void hoverLeaveEvent(QGraphicsSceneHoverEvent *event);

    // Handle mouse presses
    void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event);

private:
    Node *_node;
    NodeShapes _nodeShape;
    bool _isRoot;
    bool _isInterface;
    bool _hover;
    QString _mark;
    QPainterPath _shape;
    QRectF _boundingRect;
};

}

#endif // NODEITEM_HPP
