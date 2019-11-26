/*!
 * \file
 */
#ifndef FIRSTRUNDIALOG_HPP
#define FIRSTRUNDIALOG_HPP

#include <QDialog>

namespace Ui {
    class FirstRunDialog;
}

namespace Developer {

class Graph;

class FirstRunDialog : public QDialog
{
    Q_OBJECT

public:
    explicit FirstRunDialog(QWidget *parent = 0);
    ~FirstRunDialog();

public slots:
    void next();
    void back();
    void setPage(int page);

    void nodeAdded();
    void nodeRemoved();
    void edgeAdded();
    void edgeRemoved();

private:
    Ui::FirstRunDialog *_ui;
    Graph *_initialGraph;
    Graph *_editingGraph;
    Graph *_largeGraph;
    int _page;
    int _previousPage;
    int _nodeCount;
    int _edgeCount;
    bool _addingNodesLock;
    bool _addingEdgesLock;
    bool _deletingElementsLock;
};

}

#endif // FIRSTRUNDIALOG_HPP
