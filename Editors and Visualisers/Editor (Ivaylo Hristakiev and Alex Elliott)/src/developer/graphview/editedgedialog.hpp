/*!
 * \file
 */
#ifndef EDITEDGEDIALOG_HPP
#define EDITEDGEDIALOG_HPP

#include <QDialog>

namespace Ui {
class EditEdgeDialog;
}

namespace Developer {

class EdgeItem;
class ListValidator;

class EditEdgeDialog : public QDialog
{
    Q_OBJECT
    
public:
    explicit EditEdgeDialog(EdgeItem *edgeItem, QWidget *parent = 0);
    ~EditEdgeDialog();

    void accept();
    
private:
    Ui::EditEdgeDialog *_ui;
    EdgeItem *_edge;
    ListValidator *_labelValidator;
};

}

#endif // EDITEDGEDIALOG_HPP
