/*!
 * \file
 */
#include "graphedit.hpp"
#include "ui_graphedit.h"

namespace Developer {

GraphEdit::GraphEdit(QWidget *parent)
    : QWidget(parent)
    , _ui(new Ui::GraphEdit)
    , _graph(0)
{
    _ui->setupUi(this);

    connect(_ui->graphicsView, SIGNAL(graphHasFocus(GraphWidget*)),
            this, SLOT(handleGraphHasFocus(GraphWidget*)));
    connect(_ui->graphicsView, SIGNAL(graphLostFocus(GraphWidget*)),
            this, SLOT(handleGraphLostFocus(GraphWidget*)));
}

GraphEdit::~GraphEdit()
{
    delete _ui;
}

void GraphEdit::setGraph(Graph *graph)
{
    _ui->graphicsView->setGraph(graph);
}

void GraphEdit::handleGraphHasFocus(GraphWidget *graphWidget)
{
    emit graphHasFocus(graphWidget);
}

void GraphEdit::handleGraphLostFocus(GraphWidget *graphWidget)
{
    emit graphLostFocus(graphWidget);
}

}
