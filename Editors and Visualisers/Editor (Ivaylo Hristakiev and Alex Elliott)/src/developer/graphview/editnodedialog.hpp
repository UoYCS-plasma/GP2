/*!
 * \file
 */
#ifndef EDITNODEDIALOG_HPP
#define EDITNODEDIALOG_HPP

#include <QDialog>
#include <QRegExpValidator>

namespace Ui {
class EditNodeDialog;
}

namespace Developer {

class NodeItem;
class ListValidator;

class EditNodeDialog : public QDialog
{
    Q_OBJECT
    
public:
    explicit EditNodeDialog(NodeItem *node, QWidget *parent = 0);
    ~EditNodeDialog();

    void accept();
    
private:
    Ui::EditNodeDialog *_ui;
    NodeItem *_node;
    QRegExpValidator *_idValidator;
};

}

#endif // EDITNODEDIALOG_HPP
