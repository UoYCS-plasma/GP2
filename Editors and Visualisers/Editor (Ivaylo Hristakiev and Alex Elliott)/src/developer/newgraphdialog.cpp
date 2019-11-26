/*!
 * \file
 */
#include "newgraphdialog.hpp"
#include "ui_newgraphdialog.h"

#include "project.hpp"

#include <QFileDialog>
#include <QMessageBox>

namespace Developer {

NewGraphDialog::NewGraphDialog(Project *proj, QWidget *parent)
    : QDialog(parent)
    , _ui(new Ui::NewGraphDialog)
    , _project(proj)
{
    _ui->setupUi(this);

    // Load the help stylesheet and apply it to this widget
    QFile fp(":/stylesheets/helpdialog.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);

    // Initialise form
    // Set a default rule name as "Graph"
    // _ui->graphNameEdit->setText("Graph");
    _ui->graphNameEdit->setValidator(new QRegExpValidator(QRegExp("^(\\w|\\d|\\-|_|\\.)+$"), this));

    // Set the default directory as "${projectDir}/graphs"
    QDir dir = proj->dir();
    dir.cd("graphs");
    _ui->graphDirEdit->setText(QDir::toNativeSeparators(dir.path()));

    // Update the path field
    updatePath();
}

NewGraphDialog::~NewGraphDialog()
{
    delete _ui;
}

void NewGraphDialog::typeChanged(QString type)
{
    Q_UNUSED(type)
    updatePath();
}

void NewGraphDialog::nameChanged(QString name)
{
    Q_UNUSED(name)
    updatePath();
}

void NewGraphDialog::selectDir()
{
    QDir dir(_ui->graphDirEdit->text());
    if(!dir.exists())
        dir = _project->dir();
    QString newDir = QFileDialog::getExistingDirectory(
                this,
                tr("Select Directory"),
                dir.path()
                );
    if(!newDir.isEmpty())
    {
        _ui->graphDirEdit->setText(newDir);
        updatePath();
    }
}

void NewGraphDialog::dirChanged(QString dir)
{
    Q_UNUSED(dir)
    updatePath();
}

void NewGraphDialog::updatePath()
{
    QString fileName = _ui->graphNameEdit->text();
    GraphTypes type = DefaultGraph;

    type = AlternativeGraph;
    fileName += GP_GRAPH_ALTERNATIVE_EXTENSION;

//    if(_ui->graphTypeComboBox->currentText().startsWith("GP"))
//        type = AlternativeGraph;
//    else if(_ui->graphTypeComboBox->currentText().startsWith("Dot"))
//        type = DotGraph;
//    else if(_ui->graphTypeComboBox->currentText().startsWith("GXL"))
//        type = GxlGraph;
//    switch(type)
//    {
//    case AlternativeGraph:
//        fileName += GP_GRAPH_ALTERNATIVE_EXTENSION;
//        break;
//    case GxlGraph:
//        fileName += GP_GRAPH_GXL_EXTENSION;
//        break;
//    case DotGraph:
//    default:
//        fileName += GP_GRAPH_DOT_EXTENSION;
//        break;
//    }

    QDir dir(_ui->graphDirEdit->text());
    _ui->graphPath->setText(dir.filePath(fileName));
}

void NewGraphDialog::accept()
{
    // Ignore any empty inputs
    if(_ui->graphNameEdit->text().isEmpty()
            || _ui->graphDirEdit->text().isEmpty())
        return;

    // Check if the directory we're targetting exists, if not offer to create it
    QDir dir(_ui->graphDirEdit->text());
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

    QString fileName = _ui->graphNameEdit->text();
    GraphTypes type = DefaultGraph;

    type = AlternativeGraph;
    fileName += GP_GRAPH_ALTERNATIVE_EXTENSION;
//    if(_ui->graphTypeComboBox->currentText().startsWith("GP"))
//        type = AlternativeGraph;
//    else if(_ui->graphTypeComboBox->currentText().startsWith("Dot"))
//        type = DotGraph;
//    else if(_ui->graphTypeComboBox->currentText().startsWith("GXL"))
//        type = GxlGraph;
//    switch(type)
//    {
//    case AlternativeGraph:
//        fileName += GP_GRAPH_ALTERNATIVE_EXTENSION;
//        break;
//    case GxlGraph:
//        fileName += GP_GRAPH_GXL_EXTENSION;
//        break;
//    case DotGraph:
//    default:
//        fileName += GP_GRAPH_DOT_EXTENSION;
//        break;
//    }

    QString graph = dir.filePath(fileName);
    _project->newGraph(graph);

    QDialog::accept();
}

}
