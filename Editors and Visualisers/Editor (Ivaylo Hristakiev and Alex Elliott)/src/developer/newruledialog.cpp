/*!
 * \file
 */
#include "newruledialog.hpp"
#include "ui_newruledialog.h"

#include <QFile>
#include <QFileDialog>
#include <QMessageBox>
#include "project.hpp"


namespace Developer {

NewRuleDialog::NewRuleDialog(Project *proj, QWidget *parent)
    : QDialog(parent)
    , _ui(new Ui::NewRuleDialog)
    , _project(proj)
{
    _ui->setupUi(this);

    // Load the help stylesheet and apply it to this widget
    QFile fp(":/stylesheets/helpdialog.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);

    _ui->ruleNameEdit->setValidator(new QRegExpValidator(QRegExp("[a-z][a-zA-Z0-9]*"), this));

    // Set the default directory as "${projectDir}/rules"
    QDir dir = proj->dir();
    dir.cd("rules");
    _ui->ruleDirEdit->setText(QDir::toNativeSeparators(dir.path()));

    // Update the path field
    updatePath();
}

NewRuleDialog::~NewRuleDialog()
{
    delete _ui;
}

void NewRuleDialog::nameChanged(QString name)
{
    Q_UNUSED(name)
    updatePath();
}

void NewRuleDialog::selectDir()
{
    QDir dir(_ui->ruleDirEdit->text());
    if(!dir.exists())
        dir = _project->dir();
    QString newDir = QFileDialog::getExistingDirectory(
                this,
                tr("Select Directory"),
                dir.path()
                );
    if(!newDir.isEmpty())
    {
        _ui->ruleDirEdit->setText(newDir);
        updatePath();
    }
}

void NewRuleDialog::dirChanged(QString dir)
{
    Q_UNUSED(dir)
    updatePath();
}

void NewRuleDialog::updatePath()
{
    QString fileName = _ui->ruleNameEdit->text() + GP_RULE_EXTENSION;
    QDir dir(_ui->ruleDirEdit->text());
    _ui->rulePath->setText(dir.filePath(fileName));
}

void NewRuleDialog::accept()
{
    // Ignore any empty inputs
    if(_ui->ruleNameEdit->text().isEmpty()
            || _ui->ruleDirEdit->text().isEmpty())
        return;

    // Check if the directory we're targetting exists, if not offer to create it
    QDir dir(_ui->ruleDirEdit->text());
    if(!dir.exists())
    {
        QMessageBox::StandardButton reply;
        reply = QMessageBox::question(0, tr("Create Directory?"),
                                      tr("The directory specified (%1) does not"
                                         " exist, create it?").arg(dir.path()),
                                      QMessageBox::Yes | QMessageBox::Cancel
                                      );
        if(reply != QMessageBox::Yes)
            return;

        dir.mkpath(dir.path());
    }

    QString rule = dir.filePath(_ui->ruleNameEdit->text() + GP_RULE_EXTENSION);
    _project->newRule(rule);

    QDialog::accept();
}

}
