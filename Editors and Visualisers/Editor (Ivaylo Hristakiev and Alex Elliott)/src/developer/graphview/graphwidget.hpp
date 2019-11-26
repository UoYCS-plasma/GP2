/*!
 * \file
 */
#ifndef GRAPHWIDGET_HPP
#define GRAPHWIDGET_HPP

#include <QGraphicsView>

#include "global.hpp"

namespace Developer {

class Graph;
class GraphScene;

/*!
 * \brief Provides a top-level widget for representing a graph visualisation
 *
 * This widget provides a wrapper around the graph visualisation code in GP
 * Developer. It contains a graphics view with NodeItem and EdgeItem objects
 * tied to the Graph passed to the widget.
 */
class GraphWidget : public QGraphicsView
{
    Q_OBJECT
public:
    explicit GraphWidget(QWidget *parent = 0);

    Graph *graph() const;
    GraphScene *graphScene() const;
    void setGraph(Graph *newGraph);
    void setLinkedGraph(Graph *linkGraph);

    void layoutTree(LayoutDirections direction = DEFAULT_LAYOUT_DIRECTION);
    void layoutSugiyama();
    void layoutRadialTree();
    void layoutFPP();
    void layoutPlanarDraw();
    void layoutPlanarStraight();
    void layoutSchnyder();
    void layoutPlanarizationGrid();
    void layoutCircular();
    void layoutSpring();
    void layoutDavidsonHarel();
    void layoutFMMM();
    void layoutGEM();

protected:
    void mouseMoveEvent(QMouseEvent *event);
    void keyPressEvent(QKeyEvent *event);
    void keyReleaseEvent(QKeyEvent *event);
    void wheelEvent(QWheelEvent *event);
    void scaleView(qreal scaleFactor);

    void focusInEvent(QFocusEvent *event);
    void focusOutEvent(QFocusEvent *event);

signals:
    void graphHasFocus(GraphWidget *graphWidget);
    void graphLostFocus(GraphWidget *graphWidget);

private:
    GraphScene *_scene;
};

}

#endif // GRAPHWIDGET_HPP
