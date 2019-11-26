/*!
 * \file
 */
#ifndef IMPORTGRAPHDIALOG_HPP
#define IMPORTGRAPHDIALOG_HPP

#include <QDialog>

namespace Ui {
class ImportGraphDialog;
}

namespace Developer {

class Project;

class ImportGraphDialog : public QDialog
{
    Q_OBJECT
    
public:
    explicit ImportGraphDialog(Project *project = 0, QWidget *parent = 0);
    ~ImportGraphDialog();

public slots:
    void selectFile();
    void accept();
    
private:
    Ui::ImportGraphDialog *_ui;
    Project *_project;
};

}

#endif // IMPORTGRAPHDIALOG_HPP
