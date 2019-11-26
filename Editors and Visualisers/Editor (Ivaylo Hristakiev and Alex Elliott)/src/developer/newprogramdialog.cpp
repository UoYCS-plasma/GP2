/*!
 * \file
 */
#include "newprogramdialog.hpp"
#include "ui_newprogramdialog.h"

#include <QMessageBox>
#include <QFileDialog>

#include "project.hpp"

namespace Developer {

NewProgramDialog::NewProgramDialog(Project *proj, QWidget *parent)
    : QDialog(parent)
    , _ui(new Ui::NewProgramDialog)
    , _project(proj)
{
    _ui->setupUi(this);

    // Load the help stylesheet and apply it to this widget
    QFile fp(":/stylesheets/helpdialog.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);

    // Initialise form
    // Set a default rule name as "Program"
    //_ui->programNameEdit->setText("Program");
    _ui->programNameEdit->setValidator(new QRegExpValidator(QRegExp("^(\\w|\\d|\\-|_|\\.)+$"), this));

    // Set the default directory as "${projectDir}/programs"
    QDir dir = proj->dir();
    dir.cd("programs");
    _ui->programDirEdit->setText(QDir::toNativeSeparators(dir.path()));

    // Update the path field
    updatePath();
}

NewProgramDialog::~NewProgramDialog()
{
    delete _ui;
}

void NewProgramDialog::nameChanged(QString name)
{
    Q_UNUSED(name)
    updatePath();
}

void NewProgramDialog::selectDir()
{
    QDir dir(_ui->programDirEdit->text());
    if(!dir.exists())
        dir = _project->dir();
    QString newDir = QFileDialog::getExistingDirectory(
                this,
                tr("Select Directory"),
                dir.path()
                );
    if(!newDir.isEmpty())
    {
        _ui->programDirEdit->setText(newDir);
        updatePath();
    }
}

void NewProgramDialog::dirChanged(QString dir)
{
    Q_UNUSED(dir)
    updatePath();
}

void NewProgramDialog::updatePath()
{
    QString fileName = _ui->programNameEdit->text() + GP_PROGRAM_EXTENSION;
    QDir dir(_ui->programDirEdit->text());
    _ui->programPath->setText(dir.filePath(fileName));
}

void NewProgramDialog::accept()
{
    // Ignore any empty inputs
    if(_ui->programNameEdit->text().isEmpty()
            || _ui->programDirEdit->text().isEmpty())
        return;

    // Check if the directory we're targetting exists, if not offer to create it
    QDir dir(_ui->programDirEdit->text());
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

    QString program = dir.filePath(_ui->programNameEdit->text()
                                   + GP_PROGRAM_EXTENSION);
    _project->newProgram(program);

    QDialog::accept();
}

}
