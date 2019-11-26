/*!
 * \file
 */
#include "openprojectprogressdialog.hpp"
#include "ui_openprojectprogressdialog.h"

#include "project.hpp"

#include <QtCore>

namespace Developer {

class OpenThread : public QThread
{
public:
    OpenThread(Project *project, QObject *parent = 0)
        : QThread(parent)
        , _project(project)
    {
        _project->moveToThread(this);
    }

    ~OpenThread()
    {
        _project->moveToThread(qApp->thread());
    }

    void run()
    {
        if(!_project->open(_project->path()))
            qDebug() << "Open project failed";
        exec();
    }

private:
    Project *_project;
};

OpenProjectProgressDialog::OpenProjectProgressDialog(Project *project, QWidget *parent)
    : QDialog(parent)
    , _ui(new Ui::OpenProjectProgressDialog)
    , _project(project)
{
    _ui->setupUi(this);

    // Load the help stylesheet and apply it to this widget
    QFile fp(":/stylesheets/helpdialog.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);

    connect(_project, SIGNAL(graphListChanged()), this, SLOT(setGraphFiles()));
    connect(_project, SIGNAL(ruleListChanged()), this, SLOT(setRuleFiles()));
    connect(_project, SIGNAL(programListChanged()), this, SLOT(setProgramFiles()));
    connect(_project, SIGNAL(nodeCountChanged(int)), this, SLOT(setNodes(int)));
    connect(_project, SIGNAL(edgeCountChanged(int)), this, SLOT(setEdges(int)));
    connect(_project, SIGNAL(openComplete()), this, SLOT(projectOpened()));

    _thread = new OpenThread(_project, this);
    //_project->open(_project->path());
    _thread->start();
}

OpenProjectProgressDialog::~OpenProjectProgressDialog()
{
    delete _ui;
}

void OpenProjectProgressDialog::setGraphFiles()
{
    int count = _project->graphs().count();
    _ui->graphFiles->setText(QVariant(count).toString());
}

void OpenProjectProgressDialog::setRuleFiles()
{
    int count = _project->rules().count();
    _ui->ruleFiles->setText(QVariant(count).toString());
}

void OpenProjectProgressDialog::setProgramFiles()
{
    int count = _project->programs().count();
    _ui->programFiles->setText(QVariant(count).toString());
}

void OpenProjectProgressDialog::setNodes(int count)
{
    _ui->nodes->setText(QVariant(count).toString());
}

void OpenProjectProgressDialog::setEdges(int count)
{
    _ui->edges->setText(QVariant(count).toString());
}

void OpenProjectProgressDialog::projectOpened()
{
    _thread->exit();
    delete _thread;
    _ui->progressBar->setMaximum(100);
    _ui->progressBar->setValue(100);
    _ui->buttonBox->setEnabled(true);
    _ui->openingLabel->setText(tr("Complete."));
}

}
