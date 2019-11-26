/*!
 * \file
 */
#ifndef EDGEITEM_HPP
#define EDGEITEM_HPP

#include "nodeitem.hpp"

namespace Developer {

class Edge;

/*!
 * \brief The EdgeItem class provides a graphical representation of a graph Edge
 *  within the graph visualisation
 */
class EdgeItem : public GraphItem
{
    Q_OBJECT
public:
    explicit EdgeItem(Edge *edge, NodeItem *edgeFrom, NodeItem *edgeTo,
                      QGraphicsItem *parent = 0);
    explicit EdgeItem(const QString &edgeId, NodeItem *edgeFrom,
                      NodeItem *edgeTo, const QString &edgeLabel = QString(), const QString &edgeMark = QString(), bool isBidirectional = false,
                      QGraphicsItem *parent = 0);

    Edge *edge() const;
    NodeItem *from() const;
    NodeItem *to() const;
    QString mark() const;
    bool isBidirectional() const;

    void setFrom(NodeItem *edgeFrom);
    void setTo(NodeItem *edgeTo);
    void setLabel(const QString &itemLabel);
    void setMark(const QString &mark);
    void setBidirectional(bool isBidirectional);

    void preserveEdge();
    void deleteEdge();

    QLineF line() const;

    QPolygonF polygon(double polygonWidth = -1.0);
    QPolygonF edgePolygon(double padding = 2.0);
    QRectF boundingRect() const;
    QPainterPath shape() const;
    QPainterPath path();
    QPainterPath arrowHead(qreal adjustment = 0.0) const;
    void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
               QWidget *widget);

public slots:
    void nodeMoved();

protected:
    // Handle hover events
    void hoverEnterEvent(QGraphicsSceneHoverEvent *event);
    void hoverMoveEvent(QGraphicsSceneHoverEvent *event);
    void hoverLeaveEvent(QGraphicsSceneHoverEvent *event);

    // Handle mouse movement
    void mouseMoveEvent(QGraphicsSceneMouseEvent *event);

    // Handle mouse button events
    void mousePressEvent(QGraphicsSceneMouseEvent *event);
    void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event);
    void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);

private:
    Edge *_edge;
    QRectF _boundingRect;
    QMap<qreal, QPolygonF> _polygons;
    QPainterPath _shape;
    QPainterPath _path;
    QMap<qreal, QPainterPath> _arrowHeads;
    NodeItem *_from;
    NodeItem *_to;
    bool _isBidirectional;
    QString _mark;
    bool _hover;
    QPoint _controlPoint;   // used when drawing non-loop edges
};

}

#endif // EDGEITEM_HPP
