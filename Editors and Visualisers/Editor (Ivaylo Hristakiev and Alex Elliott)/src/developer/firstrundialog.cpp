/*!
 * \file
 */
#include "firstrundialog.hpp"
#include "ui_firstrundialog.h"

#include <QFile>

#include "graphview/graphwidget.hpp"
#include "graphview/graphscene.hpp"
#include "graph.hpp"

namespace Developer
{

FirstRunDialog::FirstRunDialog(QWidget *parent)
    : QDialog(parent)
    , _ui(new Ui::FirstRunDialog)
    , _initialGraph(0)
    , _editingGraph(0)
    , _largeGraph(0)
    , _page(0)
    , _previousPage(0)
    , _nodeCount(0)
    , _edgeCount(0)
    , _addingNodesLock(false)
    , _addingEdgesLock(false)
    , _deletingElementsLock(false)
{
    _ui->setupUi(this);

    // Load the help stylesheet and apply it to this widget
    QFile fp(":/stylesheets/helpdialog.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);

    setPage(0);
}

FirstRunDialog::~FirstRunDialog()
{
    delete _ui;
}

void FirstRunDialog::back()
{
    _previousPage = _page;
    setPage(_page-1);
}

void FirstRunDialog::next()
{
    _previousPage = _page;
    setPage(_page+1);
}

void FirstRunDialog::setPage(int page)
{
    _page = page;
    _ui->stackedWidget->setCurrentIndex(page);

    switch(page)
    {
    case 6: // large graphs
        if(_previousPage == 5)
            _largeGraph = new Graph(":/templates/example_large_graph.gv");
         _ui->graphWidget->setGraph(_largeGraph);
         _ui->backButton->setEnabled(true);
         _ui->nextButton->setVisible(false);
         _ui->nextButton->setEnabled(false);
         _ui->beginButton->setVisible(true);
         break;
    case 5: // deleting elements
        if(_previousPage == 6)
            _ui->graphWidget->setGraph(_editingGraph);
        _nodeCount = 0;
        _edgeCount = 0;
        _ui->backButton->setEnabled(true);
        _ui->nextButton->setEnabled(_deletingElementsLock);
        _ui->beginButton->setVisible(false);
        break;
    case 4: // selecting elements
    case 3: // editing elements
        _ui->backButton->setEnabled(true);
        _ui->nextButton->setEnabled(true);
        _ui->beginButton->setVisible(false);
        break;
    case 2: // adding edges
        _edgeCount = 0;
        _ui->backButton->setEnabled(true);
        _ui->nextButton->setEnabled(_addingEdgesLock);
        _ui->beginButton->setVisible(false);
        break;
    case 1: // adding nodes
        if(_previousPage == 0)
        {
            _nodeCount = 0;
            _editingGraph = new Graph();

            connect(_editingGraph, SIGNAL(nodeAdded(Node*)),
                    this, SLOT(nodeAdded()));
            connect(_editingGraph, SIGNAL(nodeRemoved(QString)),
                    this, SLOT(nodeRemoved()));
            connect(_editingGraph, SIGNAL(edgeAdded(Edge*)),
                    this, SLOT(edgeAdded()));
            connect(_editingGraph, SIGNAL(edgeRemoved(QString)),
                    this, SLOT(edgeRemoved()));

            _ui->graphWidget->setGraph(_editingGraph);
        }
        _ui->backButton->setEnabled(true);
        _ui->nextButton->setEnabled(_addingNodesLock);
        _ui->beginButton->setVisible(false);
        break;
    case 0: // introduction
    default:
        if(_initialGraph == 0)
        {
            graph_t initialGraph; // empty graph
            _initialGraph = new Graph(initialGraph);
        }
        _ui->graphWidget->setGraph(_initialGraph);
        _ui->backButton->setEnabled(false);
        _ui->nextButton->setEnabled(true);
        _ui->beginButton->setVisible(false);
        break;
    }
}

void FirstRunDialog::nodeAdded()
{
    if(_page == 1)
    {
        // We are counting nodes up to three
        ++_nodeCount;
        _ui->nodeCount->setText(QVariant(_nodeCount).toString());
        if(_nodeCount > 2)
        {
            _addingNodesLock = true;
            _ui->nodeCountIndicator->setPixmap(
                        QPixmap(":/icons/small_tick.png")
                        );
            _ui->nextButton->setEnabled(true);
        }
    }
}

void FirstRunDialog::nodeRemoved()
{
    if(_page == 5)
    {
        ++_nodeCount;
        _ui->nodeDeleteCount->setText(QVariant(_nodeCount).toString());
        if(_nodeCount > 2)
            _ui->nodeDeleteCountIndicator->setPixmap(
                        QPixmap(":/icons/small_tick.png")
                        );
        if(_nodeCount > 2 && _edgeCount > 2)
        {
            _deletingElementsLock = true;
            _ui->nextButton->setEnabled(true);
        }
    }
}

void FirstRunDialog::edgeAdded()
{
    if(_page == 2)
    {
        // We are counting edges up to three
        ++_edgeCount;
        _ui->edgeCount->setText(QVariant(_edgeCount).toString());
        if(_edgeCount > 2)
        {
            _addingEdgesLock = true;
            _ui->edgeCountIndicator->setPixmap(
                        QPixmap(":/icons/small_tick.png")
                        );
            _ui->nextButton->setEnabled(true);
        }
    }
}

void FirstRunDialog::edgeRemoved()
{
    if(_page == 5)
    {
        ++_edgeCount;
        _ui->edgeDeleteCount->setText(QVariant(_edgeCount).toString());
        if(_edgeCount > 2)
            _ui->edgeDeleteCountIndicator->setPixmap(
                        QPixmap(":/icons/small_tick.png")
                        );
        if(_nodeCount > 2 && _edgeCount > 2)
        {
            _deletingElementsLock = true;
            _ui->nextButton->setEnabled(true);
        }
    }
}

}
