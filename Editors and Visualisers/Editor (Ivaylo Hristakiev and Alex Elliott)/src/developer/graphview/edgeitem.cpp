/*!
 * \file
 */
#include "edgeitem.hpp"
#include "edge.hpp"
#include "editedgedialog.hpp"

#include "graph.hpp"

#include <QApplication>
#include <QPainter>
#include <QStyleOptionGraphicsItem>
#include <QSettings>
#include <QGraphicsSceneMouseEvent>
#include <QDebug>
#include <QGraphicsScene>
#include <QSettings>

namespace Developer {

EdgeItem::EdgeItem(Edge *edge, NodeItem *edgeFrom, NodeItem *edgeTo, QGraphicsItem *parent)
    : GraphItem(edge->id(), edge->label(), "edge", parent)
    , _edge(edge)
    , _from(edgeFrom)
    , _to(edgeTo)
    , _hover(false)
    , _mark(edge->mark())
    , _isBidirectional(edge->isBidirectional())
{
    setZValue(EDGE_Z_VALUE);

    setAcceptHoverEvents(true);
    setFlags(QGraphicsItem::ItemIsSelectable);

    setFrom(edgeFrom);
    setTo(edgeTo);

    nodeMoved();
}

EdgeItem::EdgeItem(const QString &edgeId, NodeItem *edgeFrom, NodeItem *edgeTo,
                   const QString &edgeLabel, const QString &edgeMark, bool isBidirectional, QGraphicsItem *parent)
    : GraphItem(edgeId, edgeLabel, "edge", parent)
    , _edge(0)
    , _from(edgeFrom)
    , _to(edgeTo)
    , _hover(false)
    , _mark(edgeMark)
    , _isBidirectional(isBidirectional)
{
    setZValue(EDGE_Z_VALUE);

    setAcceptHoverEvents(true);
    setFlags(QGraphicsItem::ItemIsSelectable);

    setFrom(edgeFrom);
    setTo(edgeTo);

    nodeMoved();
}

Edge *EdgeItem::edge() const
{
    return _edge;
}

NodeItem *EdgeItem::from() const
{
    return _from;
}

NodeItem *EdgeItem::to() const
{
    return _to;
}

bool EdgeItem::isBidirectional() const
{
    return _isBidirectional;
}

QString EdgeItem::mark() const
{
    return _mark;
}

void EdgeItem::setMark(const QString &mark)
{
    _mark = mark;

    if(_edge != 0)
        _edge->setMark(mark);
}

void EdgeItem::setBidirectional(bool isBidirectional)
{
    _isBidirectional = isBidirectional;

    if(_edge != 0)
        _edge->setIsBidirectional(isBidirectional);
}

void EdgeItem::setFrom(NodeItem *edgeFrom)
{
    // Assign the node this edge is from and ensure that it sends a signal to
    // existing edges that they may need to recalculate
    _from = edgeFrom;
    _from->addedEdge();

    // Ensure that this edge is notified of any changes which may affect this
    // edge's presentation
    connect(_from, SIGNAL(edgeAdded()), this, SLOT(nodeMoved()));
    connect(_from, SIGNAL(xChanged()), this, SLOT(nodeMoved()));
    connect(_from, SIGNAL(yChanged()), this, SLOT(nodeMoved()));
    connect(_from, SIGNAL(shapeChanged()), this, SLOT(nodeMoved()));
}

void EdgeItem::setTo(NodeItem *edgeTo)
{
    // Assign the node this edge is to and ensure that it sends a signal to
    // existing edges that they may need to recalculate
    _to = edgeTo;
    _to->addedEdge();

    // Ensure that this edge is notified of any changes which may affect this
    // edge's presentation
    connect(_to, SIGNAL(edgeAdded()), this, SLOT(nodeMoved()));
    connect(_to, SIGNAL(xChanged()), this, SLOT(nodeMoved()));
    connect(_to, SIGNAL(yChanged()), this, SLOT(nodeMoved()));
    connect(_to, SIGNAL(shapeChanged()), this, SLOT(nodeMoved()));
}

void EdgeItem::setLabel(const QString &itemLabel)
{
    GraphItem::setLabel(itemLabel);

    if(_edge != 0)
        _edge->setLabel(itemLabel);
}

void EdgeItem::deleteEdge()
{
    setItemState(GraphItem::GraphItem_Deleted);

    if(_edge != 0)
        _edge->setPhantom(true);
}

void EdgeItem::preserveEdge()
{
    setItemState(GraphItem::GraphItem_Normal);

    if(_edge != 0)
        _edge->setPhantom(false);
}

QLineF EdgeItem::line() const
{
    QLineF initial = QLineF(_from->centerPos(), _to->centerPos());
    QList<QPointF> fromIntersection = _from->intersection(initial);
    QList<QPointF> toIntersection = _to->intersection(initial);

    if(fromIntersection.count() < 1 || toIntersection.count() < 1)
        return initial;

    return QLineF(fromIntersection.at(0), toIntersection.at(0));
}

QPolygonF EdgeItem::polygon(double polygonWidth)
{
    if(_polygons.contains(polygonWidth))
        return _polygons[polygonWidth];

    QSettings settings;
    QFont font = settings.value("GraphView/Edges/Font", qApp->font()
                                ).value<QFont>();
    qreal arrowSize = settings.value("GraphView/Edges/ArrowSize", 9).toDouble();
    QFontMetrics metrics(font);

    // Determine if path() is drawing a straight edge, a curved edge or an ellipse (loop edge)
    bool straightLine = false;
    bool curvedLine = false;
    bool loop = false;

    if(_from != _to)
    {
        // We have drawn a line/curve edge

        QPoint edgeUpperControlPoint = _controlPoint;
        QPoint edgeLowerControlPoint = _controlPoint;

        // Obtain the point between the two nodes. All other lines should curve with respect to this point
        QLineF edgeLine = line();
        QPointF midPoint((edgeLine.p1().x() + edgeLine.p2().x())/2,
                         (edgeLine.p1().y() + edgeLine.p2().y())/2);
        // The length of this line defines how much is the edge curved
        QLineF opposingLine(midPoint, edgeLine.p2());
        opposingLine.setLength(32 / 2);

        // Calculate which side to rotate the perpendicular line
        qreal angle = opposingLine.angle();
        angle += 90; if(angle > 360) angle -= 360;
        opposingLine.setAngle(angle);

        edgeUpperControlPoint.setX(edgeUpperControlPoint.x() + opposingLine.dx());
        edgeUpperControlPoint.setY(edgeUpperControlPoint.y() + opposingLine.dy());

        edgeLowerControlPoint.setX(edgeLowerControlPoint.x() - opposingLine.dx());
        edgeLowerControlPoint.setY(edgeLowerControlPoint.y() - opposingLine.dy());


        QPainterPath painterPath(edgeLine.p1());
        painterPath.quadTo(edgeLowerControlPoint, edgeLine.p2());
        painterPath.quadTo(edgeUpperControlPoint, edgeLine.p1());
        return painterPath.toFillPolygon();
    }
    else
    {
        return path().toFillPolygon();
        // This is a loop edge
        // We should draw an ellipse

        QPainterPath nodeShape = _from->shape();
        nodeShape.translate(_from->scenePos());
        QPointF center = _from->centerPos();
        center.setY(center.y() - ((nodeShape.boundingRect().height()/2)
                                  + 10));

        // Doubled for consistency with the above method
        if(polygonWidth <= 0)
            polygonWidth = 2*metrics.height() + 4.0;

        // Build a larger outside loop
        QPainterPath outerLoop(center);
        outerLoop.addEllipse(
                    center,
                    15 + polygonWidth/2,
                    20 + polygonWidth/2);
        QPolygonF outerPolygon = outerLoop.toFillPolygon();

        QPainterPath innerLoop(center);
        if(polygonWidth/2 > 15)
            innerLoop.addEllipse(
                        center,
                        15 - arrowSize/2,
                        20 - arrowSize/2);
        else
            innerLoop.addEllipse(
                        center,
                        15 - polygonWidth/2,
                        20 - polygonWidth/2);
        QPolygonF innerPolygon = innerLoop.toFillPolygon();

        QPainterPath fromShape = _from->shape();
        fromShape.translate(_from->pos());

        QPolygonF result = outerPolygon.subtracted(fromShape.toFillPolygon());
        result = result.subtracted(innerPolygon);
        return result;
    }
}

QPolygonF EdgeItem::edgePolygon(double padding)
{
    // This method works exactly like the above one, except that the width of
    // the produced polygon is just sufficient to contain the arrow itself
    QSettings settings;
    qreal arrowSize = settings.value("GraphView/Edges/ArrowSize", 9).toDouble();
    return polygon(arrowSize+(2*padding));
}

QRectF EdgeItem::boundingRect() const
{
    return _boundingRect;
}

QPainterPath EdgeItem::shape() const
{
    if(!_shape.isEmpty())
        return _shape;
    else
        return QPainterPath();
}

QPainterPath EdgeItem::path()
{
    if(!_path.isEmpty())
        return _path;

    QSettings settings;
    qreal lineWidth = settings.value("GraphView/Edges/LineWidth", 1.5).toDouble();

    if(_from != _to)
    {
        QLineF edgeLine = line();
        // Is there an edge in the other direction?
        bool oppositeEdge = _from->node()->parent()->hasEdgeFromTo(_to->id(), _from->id());

        // There is a loop in the other direction, curve this one to avoid
        // it - the other one should also curve producing a gap
        QPointF midPoint((edgeLine.p1().x() + edgeLine.p2().x())/2,
                         (edgeLine.p1().y() + edgeLine.p2().y())/2);

        // The length of this line defines how much is the edge curved
        QLineF opposingLine(midPoint, edgeLine.p2());
        opposingLine.setLength(32);

        // Calculate which side to rotate the perpendicular line
        qreal angle = opposingLine.angle();
        angle += 90; if(angle > 360) angle -= 360;
        opposingLine.setAngle(angle);

        // Is there an edge in the same direction?
        std::vector<Edge *> edges;

        if(_to->node() == 0)
            edges = std::vector<Edge *>();
        else
            edges = _edge->parent()->edgesFromTo(_from->id(), _to->id());   // obtain the parallel edges

        qreal positionIncrement = 0;
        if (edges.size() >= 2)
        {
            // There are multiple edges in the same direction
            for (std::vector<Edge *>::const_iterator it = edges.begin(); it != edges.end(); ++it)
            {
                Edge* otherEdge = *it;
                if (otherEdge != _edge)
                    // keep incrementing until we find this edge in the collection of parallel edges
                    positionIncrement+=32;
                else
                    break;
            }
            //qDebug() << "  edgeitem.cpp: ("<< _edge->id() << ") Edge in the same direction detected, increment: " << positionIncrement;
        }

        opposingLine.setLength(opposingLine.length() + positionIncrement);


        if (!oppositeEdge)
        {
            // no curve
            opposingLine.setLength(opposingLine.length() - 32);

            // modify the line length to compensate for pen width
            edgeLine.setLength(edgeLine.length()-(lineWidth+0.5));
        }

        _controlPoint = QPoint(opposingLine.p2().x(),opposingLine.p2().y());
        QPainterPath painterPath(edgeLine.p1());
        painterPath.quadTo(opposingLine.p2(), edgeLine.p2());

        return painterPath;
    }
    else
    {
        // This is a loop edge
        //! \todo Extend this to try North, East, South and West aligned loops
        //!     to try to minimise edge crossing
        QPainterPath nodeShape = _from->shape();
        nodeShape.translate(_from->scenePos());
        QPointF center = _from->centerPos();


        // Is there an edge in the same direction?
        std::vector<Edge *> edges;

        if(_to->node() == 0)
            edges = std::vector<Edge *>();
        else
            edges = _edge->parent()->edgesFromTo(_from->id(), _to->id());   // obtain the parallel edges

        qreal positionIncrement = 0;
        if (edges.size() >= 2)
        {
            if (edges.size() > 10)
                qDebug() << "  Warning: Attempting to draw more than 10 parallel loops";
            // There are multiple edges in the same direction
            for (std::vector<Edge *>::const_iterator it = edges.begin(); it != edges.end(); ++it)
            {
                Edge* otherEdge = *it;
                if (otherEdge != _edge)
                    // keep incrementing until we find this edge in the collection of parallel edges
                    if (edges.size() >= 4)
                        positionIncrement+=8;
                    else
                        positionIncrement+=10;
                else
                    break;
            }
            // qDebug() << "  edgeitem.cpp: ("<< _edge->id() << ") Loop in the same direction detected, increment: " << positionIncrement;
        }

        // Move the ellipse center up by it's 2nd radius units
        center.setY(center.y() - (20 + positionIncrement));

        QPainterPath painterPath(center);
        painterPath.addEllipse(center, 15 + positionIncrement/3, 20 + positionIncrement);

        QRectF loopBoundingRect = painterPath.boundingRect();
        QPolygonF pathPolygon = painterPath.toFillPolygon();
        QPolygonF fromPolygon = nodeShape.toFillPolygon();

        QList<QPointF> intersections;
        for(int i = 1; i < pathPolygon.count(); ++i)
        {
            for(int j = 1; j < fromPolygon.count(); ++j)
            {
                QLineF l1 = QLineF(pathPolygon.at(i-1), pathPolygon.at(i));
                QLineF l2 = QLineF(fromPolygon.at(j-1), fromPolygon.at(j));

                QPointF intersection;
                if(l1.intersect(l2, &intersection)
                        == QLineF::BoundedIntersection)
                    intersections << intersection;
            }
        }

        bool unattachedLoop = false;
        if(intersections.count() < 2)
        {
            qDebug() << "Warning: The loop (" << _id << ") became too big to be drawn sensibly";
            unattachedLoop = true;
        }

        QPointF arcStartPoint;
        QPointF arcEndPoint;
        if(intersections.at(0).x() < intersections.at(1).x())
        {
            arcStartPoint = intersections.at(1);
            arcEndPoint = intersections.at(0);
        }
        else
        {
            arcStartPoint = intersections.at(0);
            arcEndPoint = intersections.at(1);
        }

        QLineF arcStartLine(center, arcStartPoint);
        QLineF arcEndLine(center, arcEndPoint);
        qreal sweepLength = arcStartLine.angle() - arcEndLine.angle();
        if(sweepLength < 0) sweepLength += 360;
        sweepLength = 360.0 - sweepLength;

        painterPath = QPainterPath(arcStartPoint);
        painterPath.arcTo(loopBoundingRect, arcStartLine.angle(), sweepLength);
        return painterPath;
    }
}

QPainterPath EdgeItem::arrowHead(qreal adjustment) const
{
    if(_arrowHeads.contains(adjustment))
        return _arrowHeads[adjustment];

    QSettings settings;
    qreal arrowSize = settings.value("GraphView/Edges/ArrowSize", 9).toDouble();

    QLineF drawLine(_path.pointAtPercent(.95 + adjustment),
                    _path.pointAtPercent(1.0 + adjustment));
    QPainterPath painterPath(_path.pointAtPercent(1.0 + adjustment));
    // Abuse QLineF's ability to transform lines in order to draw a triangle
    // at the end of this edge as follows:

    // Get the current angle
    qreal angle = drawLine.angle();
    // Shift it around by 150 degrees, this leaves 30 degrees as the internal
    // angle which is what we want as the edge cuts the equilateral triangle's
    // 60 degree internal angles in half
    // If we overshoot and get negative simply add 360 to fix it
    angle -= 150; if(angle < 0) angle += 360;
    // Move the line such that p1 is now where p2 sat
    drawLine.translate(drawLine.p2()-drawLine.p1());
    // Resize the line to be the length of an edge of the triangle
    drawLine.setLength(arrowSize);
    // Set the new angle, p2 is now in the correct place
    drawLine.setAngle(angle);
    // Draw a line to this point
    painterPath.lineTo(drawLine.p2());

    // Now that we've done the initial line the rest are all relative to the
    // equilateral triangle, and therefore we are moving to produce 60 degree
    // internal angles
    angle -= 120; if(angle < 0) angle += 360;
    // The rest is exactly the same
    drawLine.translate(drawLine.p2()-drawLine.p1());
    drawLine.setLength(arrowSize);
    drawLine.setAngle(angle);
    painterPath.lineTo(drawLine.p2());

    // One last time to get us back to our origin
    angle -= 120; if(angle < 0) angle += 360;
    drawLine.translate(drawLine.p2()-drawLine.p1());
    drawLine.setLength(arrowSize);
    drawLine.setAngle(angle);
    painterPath.lineTo(drawLine.p2());

    return painterPath;
}

void EdgeItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                     QWidget *widget)
{
    Q_UNUSED(widget)

    QSettings settings;
    QFont font = settings.value("GraphView/Edges/Font", qApp->font()
                                ).value<QFont>();
    qreal lineWidth = settings.value("GraphView/Edges/LineWidth", 1.5).toDouble();
    QFontMetrics metrics(font);
    QColor textColour = settings.value("GraphView/Edges/TextColour",
                                       QColor(0x33, 0x33, 0x33)).value<QColor>();
    QColor lineColour;
    if(option->state & QStyle::State_Selected)
    {
        lineColour  = settings.value("GraphView/Edges/SelectedColour",
                                     QColor(0xcc,0xcc,0x33)).value<QColor>();
    }
    else
    {
        // default
        lineColour = settings.value("GraphView/Edges/Background",
                             QColor(0x11,0x11,0x11) // black
                             ).value<QColor>();

        if (_mark == "red")
            lineColour = settings.value("GraphView/Edges/ColourRed",
                                 QColor(0xee,0x77,0x77) // light red
                                 ).value<QColor>();

        if (_mark == "green")
            lineColour = settings.value("GraphView/Edges/ColourGreen",
                                 QColor(0x60,0xcb,0x60) // green
                                 ).value<QColor>();

        if (_mark == "blue")
            lineColour = settings.value("GraphView/Edges/ColourBlue",
                                 QColor(0x22,0x70,0xee) // blue
                                 ).value<QColor>();

        if (_mark == "any")
            lineColour = settings.value("GraphView/Edges/ColourAny",
                                 QColor(0xee,0x82,0xee) // purple-ish
                                 ).value<QColor>();

        if (_hover)
        {
            lineColour = lineColour.lighter(110);
        }
    }

    if(SHOW_VISUALISATION_DEBUG)
    {
        painter->setPen(DEBUG_COLOUR);
        painter->drawRect(boundingRect());
        painter->drawPolygon(polygon());
        painter->drawPolygon(edgePolygon());
    }

    QPen pen(lineColour);
    pen.setWidth(lineWidth);
    if (_mark == "dashed")
    {
        pen.setStyle(Qt::DashLine);
    }
    painter->setPen(pen);
    painter->setBrush(Qt::NoBrush);
    painter->setFont(font);

    // Draw the line
    QPainterPath painterPath = path();
    painter->drawPath(painterPath);

    // Draw the pointer on the end
    // ONLY IF EDGE IS NOT BIDIRECTIONAL
    if (!_isBidirectional)
    {
        QPainterPath arrowPath;
        if(_from == _to)
            arrowPath = arrowHead(-0.05);
        else
            arrowPath = arrowHead();
        painter->setBrush(lineColour);
        painter->drawPath(arrowPath);
    }


    // Now draw the label
    painter->setPen(textColour);
    QPointF midPoint = painterPath.pointAtPercent(.5);
    painter->translate(midPoint);
    qreal angle = painterPath.angleAtPercent(.5);
    if(angle > 90 && angle < 270) { angle -= 180; }
    if(angle < 0) angle += 360;
    painter->rotate(-angle);
    qreal xOffset = metrics.width(label())/2;
    painter->drawText(QPointF(-xOffset,-3), label());

    // Draw the edge ID
    /*
    QColor idColour = textColour;
    idColour.setAlpha(80);
    painter->setPen(idColour);
    xOffset = metrics.width(id())/2;
    qreal yOffset = metrics.height()-3;
    painter->drawText(QPointF(-xOffset, yOffset), id());
    */
}

void EdgeItem::nodeMoved()
{
    QSettings settings;
    qreal arrowSize = settings.value("GraphView/Edges/ArrowSize", 9).toDouble();
    _path = QPainterPath();
    _path = path();
    _arrowHeads.clear();
    _arrowHeads.insert(0.0, arrowHead());
    _arrowHeads.insert(-0.05, arrowHead(-0.05));
    _polygons.clear();
    _polygons.insert(-1.0, polygon());
    _polygons.insert(arrowSize, polygon(arrowSize));
    _shape = QPainterPath();
    _shape.addPolygon(polygon());
    _boundingRect = _polygons[-1.0].boundingRect();
    update();
}

void EdgeItem::hoverEnterEvent(QGraphicsSceneHoverEvent *event)
{
    if(edgePolygon().containsPoint(event->scenePos(), Qt::OddEvenFill))
    {
        event->accept();
        _hover = true;
        //qDebug() << "  edgeitem.cpp: Edge "<<_id << " has hover focus (1)";
    }
    else
    {
        event->ignore();
        _hover = false;
        //qDebug() << "  edgeitem.cpp: Edge "<<_id << " has LOST hover focus (2)";
    }

    update();
}

void EdgeItem::hoverMoveEvent(QGraphicsSceneHoverEvent *event)
{
    if(edgePolygon().containsPoint(event->scenePos(), Qt::OddEvenFill))
    {
        event->accept();
        _hover = true;
        //qDebug() << "  edgeitem.cpp: Edge "<<_id << " has hover focus (2)";
    }
    else
    {
        event->ignore();
        _hover = false;
    }

    update();
}

void EdgeItem::hoverLeaveEvent(QGraphicsSceneHoverEvent *event)
{
    Q_UNUSED(event)
    _hover = false;

    update();
}

void EdgeItem::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
    QGraphicsItem::mouseMoveEvent(event);
}

void EdgeItem::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    if(event->button() == Qt::LeftButton && _hover)
    {
        event->accept();
        scene()->clearSelection();
        setSelected(true);
    }
    else
    {
        event->ignore();
        QGraphicsItem::mousePressEvent(event);
    }
}

void EdgeItem::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event)
{
    if(edge()->parent()->status() == GPFile::ReadOnly)
    {
        QGraphicsItem::mouseDoubleClickEvent(event);
        return;
    }

    if(event->button() == Qt::LeftButton && _hover)
    {
        event->accept();
        EditEdgeDialog dialog(this);
        dialog.exec();
    }
    else
        QGraphicsItem::mouseDoubleClickEvent(event);
}

void EdgeItem::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
    QGraphicsItem::mouseReleaseEvent(event);
}

}
