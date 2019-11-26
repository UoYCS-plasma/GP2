/*!
 * \file
 */
#ifndef NEWRULEDIALOG_HPP
#define NEWRULEDIALOG_HPP

#include <QDialog>

namespace Ui {
class NewRuleDialog;
}

namespace Developer {

class Project;

class NewRuleDialog : public QDialog
{
    Q_OBJECT
    
public:
    explicit NewRuleDialog(Project *proj, QWidget *parent = 0);
    ~NewRuleDialog();

public slots:
    void nameChanged(QString name);
    void selectDir();
    void dirChanged(QString dir);

    void updatePath();

    void accept();
    
private:
    Ui::NewRuleDialog *_ui;
    Project *_project;
};

}

#endif // NEWRULEDIALOG_HPP
