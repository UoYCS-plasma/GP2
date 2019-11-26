/*!
 * \file
 */
#ifndef IMPORTRULEDIALOG_HPP
#define IMPORTRULEDIALOG_HPP

#include <QDialog>

namespace Ui {
class ImportRuleDialog;
}

namespace Developer {

class Project;

class ImportRuleDialog : public QDialog
{
    Q_OBJECT
    
public:
    explicit ImportRuleDialog(Project *project = 0, QWidget *parent = 0);
    ~ImportRuleDialog();

public slots:
    void selectFile();
    void accept();
    
private:
    Ui::ImportRuleDialog *_ui;
    Project *_project;
};

}

#endif // IMPORTRULEDIALOG_HPP
