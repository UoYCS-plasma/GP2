/*!
 * \file
 */
#include "importruledialog.hpp"
#include "ui_importruledialog.h"

#include <QFile>
#include <QFileDialog>
#include <QSettings>
#include <QMessageBox>
#include "project.hpp"

namespace Developer {

ImportRuleDialog::ImportRuleDialog(Project *project, QWidget *parent)
    : QDialog(parent)
    , _ui(new Ui::ImportRuleDialog)
    , _project(project)
{
    _ui->setupUi(this);

    // Load the help stylesheet and apply it to this widget
    QFile fp(":/stylesheets/helpdialog.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);
}

ImportRuleDialog::~ImportRuleDialog()
{
    delete _ui;
}

void ImportRuleDialog::selectFile()
{
    QString file = _ui->ruleFileEdit->text();
    QFileInfo info(file);

    QSettings settings;
    QString defaultPath = settings.value(
                "Projects/DefaultProjectLocation",
                QVariant(QDir::toNativeSeparators(
                             QDir::homePath()
                             ))
                ).toString();

    QString dir;
    if(info.exists())
        dir = info.dir().path();
    else
        dir = defaultPath;

    file = QFileDialog::getOpenFileName(
                this,
                tr("Import Rule File"),
                dir,
                tr("GP Rule Files (*.gpr)"));

    if(!file.isEmpty())
        _ui->ruleFileEdit->setText(file);
}

void ImportRuleDialog::accept()
{
    QString path = _ui->ruleFileEdit->text();
    QFileInfo info(path);
    if(path.isEmpty() || !info.exists())
    {
        QMessageBox::warning(
                    this,
                    tr("Cannot Import File"),
                    tr("The file provided is invalid or does not exist, cannot "
                       "import the file into the project."));
        return;
    }

    if(_ui->copyCheckBox->isChecked())
    {
        QDir dir = _project->rulesDir();
        QString newPath = dir.filePath(info.fileName());

        int i = 1;
        while(QFile(newPath).exists())
        {
            newPath = dir.filePath(info.baseName() + "_" +
                                   QVariant(i).toString() + "." + info.suffix());
            ++i;
        }

        QFile file(path);
        if(file.copy(newPath))
        {
            _project->addRule(newPath);
            QDialog::accept();
        }
        else
        {
            QMessageBox::warning(
                        this,
                        tr("Failed to Import File"),
                        tr("GP Developer could not copy the file at '%1' to "
                           "'%2' - unknown error, perhaps a file already exists "
                           "there").arg(path, newPath));
            QDialog::reject();
        }
    }
    else
    {
        _project->addRule(path);
        QDialog::accept();
    }
}

}
