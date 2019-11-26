/*!
 * \file
 */
#include "newprojectwizard.hpp"
#include "ui_newprojectwizard.h"

#include <QFileDialog>
#include <QSettings>
#include <QRegExp>
#include <QMessageBox>

namespace Developer {

NewProjectWizard::NewProjectWizard(QWidget *parent)
    : QWizard(parent)
    , _ui(new Ui::NewProjectWizard)
    , _project(0)
    , _fileNameValidation("^(\\w|\\d|\\-|_|\\.)+$")
{
    _ui->setupUi(this);

    #ifndef Q_OS_WIN32
        // The Linux and OSX styles do not fit in with the rest of the application,
        // however the Aero style looks pretty good with the existing stylesheet.
        setWizardStyle(QWizard::ModernStyle);
    #endif // Q_OS_WIN32

    QSettings settings;
    QString path = settings.value("Projects/DefaultProjectLocation", QVariant(QDir::toNativeSeparators(QDir::homePath()))).toString();

    _ui->projectLocationEdit->setText(path);
    _ui->createdPathLabel->setText(path);

    _ui->projectNameEdit->setValidator(new QRegExpValidator(_fileNameValidation, this));
}

NewProjectWizard::~NewProjectWizard()
{
    delete _ui;
}

Project *NewProjectWizard::project() const
{
    return _project;
}

bool NewProjectWizard::validateCurrentPage()
{
    switch(currentId())
    {
        case 0:
            {
                // Validate the target directory
                QDir dir;
                if(!dir.exists(_ui->projectLocationEdit->text()))
                {
                    QMessageBox::StandardButton reply;
                    reply = QMessageBox::question(0, tr("Create Directory?"),
                                                    tr("The directory specified (%1) does not"
                                                    " exist, create it?").arg(_ui->projectLocationEdit->text()),
                                                    QMessageBox::Yes | QMessageBox::Cancel
                    );

                    if(reply != QMessageBox::Yes)
                        return false;
                    else
                    {
                        // If this fails we can't make it
                        if(!dir.mkpath(_ui->projectLocationEdit->text()))
                        {
                            QMessageBox::warning(
                            this,
                            tr("Directory Creation Failed"),
                            tr("Could not create the specified path")
                            );
                            return false;
                        }

                        return dir.exists(_ui->projectLocationEdit->text());
                    }
                }
            }

            // Validate the project name
            if  (_ui->projectNameEdit->text() == QString()
                || !_fileNameValidation.exactMatch(_ui->projectNameEdit->text())
                )
                return false;

            break;
        default:
            // If given no other information, return true by default
            return true;
    }

    // No reason to fail found, accept the data
    return true;
}

void NewProjectWizard::selectProjectLocation()
{
    QFileDialog fileDialog(this);
    QString dir = fileDialog.getExistingDirectory(this, tr("Select a Project Directory"), _ui->projectLocationEdit->text());
    _ui->projectLocationEdit->setText(QDir::toNativeSeparators(dir));
}

void NewProjectWizard::updateProjectLocation()
{
    _projectPath = QDir::toNativeSeparators(_ui->projectLocationEdit->text());
    if(!_projectPath.endsWith(QDir::separator()))
        _projectPath += QDir::separator();

    _projectPath += _ui->projectNameEdit->text();

    _ui->createdPathLabel->setText(_projectPath);
}

void NewProjectWizard::accept()
{
    // Check if the default project location should be updated (even if it's
    // with the same value for simplicity's sake)
    if(_ui->updateDefaultLocationCheckBox->isChecked())
    {
        QSettings settings;
        settings.setValue(
                    "Projects/DefaultProjectLocation",
                    _ui->projectLocationEdit->text()
                    );
    }

    GPVersions version = GP2;

    // Set up the new project
    _project = new Project();
    _project->initProject(
                _projectPath,
                _ui->projectNameEdit->text(),
                version
                );

    // Add a set of initial files
//    _project->newGraph(QString("Graph1.gpg"));
//    _project->newProgram(QString("Program1"));
//    _project->newRule(QString("Rule1"));

    QDialog::accept();
}

}
