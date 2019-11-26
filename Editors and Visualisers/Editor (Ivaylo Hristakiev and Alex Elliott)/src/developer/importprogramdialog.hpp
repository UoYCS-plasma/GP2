/*!
 * \file
 */
#ifndef IMPORTPROGRAMDIALOG_HPP
#define IMPORTPROGRAMDIALOG_HPP

#include <QDialog>

namespace Ui {
class ImportProgramDialog;
}

namespace Developer {

class Project;

class ImportProgramDialog : public QDialog
{
    Q_OBJECT
    
public:
    explicit ImportProgramDialog(Project *project = 0, QWidget *parent = 0);
    ~ImportProgramDialog();

public slots:
    void selectFile();
    void accept();
    
private:
    Ui::ImportProgramDialog *_ui;
    Project *_project;
};

}

#endif // IMPORTPROGRAMDIALOG_HPP
