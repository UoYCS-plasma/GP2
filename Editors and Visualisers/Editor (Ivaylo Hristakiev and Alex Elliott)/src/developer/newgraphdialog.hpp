/*!
 * \file
 */
#ifndef NEWGRAPHDIALOG_HPP
#define NEWGRAPHDIALOG_HPP

#include <QDialog>

namespace Ui {
class NewGraphDialog;
}

namespace Developer {

class Project;

class NewGraphDialog : public QDialog
{
    Q_OBJECT
    
public:
    explicit NewGraphDialog(Project *proj, QWidget *parent = 0);
    ~NewGraphDialog();

public slots:
    void typeChanged(QString type);
    void nameChanged(QString name);
    void selectDir();
    void dirChanged(QString dir);

    void updatePath();

    void accept();
    
private:
    Ui::NewGraphDialog *_ui;
    Project *_project;
};

}

#endif // NEWGRAPHDIALOG_HPP
