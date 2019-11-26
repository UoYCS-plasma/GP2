/*!
 * \file
 */
#include "run.hpp"
#include "ui_run.h"

#include "project.hpp"
#include "runconfiguration.hpp"

namespace Ui {
    class RunConfiguration;
}

namespace Developer {

Run::Run(QWidget *parent)
    : QWidget(parent)
    , _ui(new Ui::Run)
    , _initial(true)
{
    _ui->setupUi(this);
}

Run::~Run()
{
    delete _ui;
}

Project *Run::project() const
{
    return _project;
}

void Run::setProject(Project *proj)
{
    if(proj == 0)
    {
        qDebug() << "Run::setProject() handed null pointer, ignoring";
        return;
    }

    //disconnect(_project, SIGNAL(runConfigurationListChanged()), this, SLOT(handleRunConfigListChanged()));
    _project = proj;

    if ((!_ui) || (!_ui->runConfigurations) || (!_ui->runConfigurations->layout()))
    {
        qDebug() << "Error: Run UI not initialized; ignoring";
        return;
    }

    QLayoutItem *item;
    while((item = _ui->runConfigurations->layout()->takeAt(0)) != 0)
    {
        delete item->widget();
        delete item;
    }

    QVector<RunConfig *> runConfigs = _project->runConfigurations();
    if(runConfigs.size() < 1)
    {
        _initial = true;
        QLabel *label = new QLabel(tr("There are currently no run "
                                      "configurations set, click the button "
                                      "below to add one"),
                                   _ui->runConfigurations);
        _ui->runConfigurations->layout()->addWidget(label);
    }
    else
    {
        // Populate with run configurations
        for (int i = 0; i < runConfigs.size(); i++)
        {
            RunConfig* config = runConfigs.at(i);
            // qDebug () << "  Run: configuration detected" << config->name() << config->program() << config->graph();

            // Creates the widget and adds it to the UI
            this->addRunConfiguration(config);
        }
    }
    _ui->addRunConfigurationButton->setEnabled(true);

    connect(_project, SIGNAL(runConfigurationListChanged()), this, SLOT(handleRunConfigListChanged()));
}

void Run::handleResultGraph(QString resultLocation, RunConfig* runConfig)
{
    emit obtainedResultGraph(resultLocation, runConfig);
}

void Run::handleRunConfigListChanged()
{
    QVector<RunConfig*> configs = _project->runConfigurations();
   //  qDebug () << "  Run: Detected" << configs.size() << "configs";


//    QLayoutItem *item;
//    while((item = _ui->runConfigurations->layout()->takeAt(0)) != 0)
//    {
//        delete item->widget();
//        delete item;
//    }

    // If we are not at the start (project was just opened), then we already have the
    // full list of run configs (project owned and local)
    // No need to update in that case
    if (!_initial)
        return;

    for (int i = 0; i < configs.size(); i++)
    {
        RunConfig* config = configs.at(i);
        // qDebug () << "  Run: configuration detected" << config->name() << config->program() << config->graph();

        RunConfiguration* runConfig = this->addRunConfiguration(config);
        runConfig->setName(config->name());
        runConfig->setProgram(config->program());
        runConfig->setGraph(config->graph());
    }
}

RunConfiguration *Run::addRunConfiguration(RunConfig* runConfig)
{
    if(_initial)
    {
        _initial = false;

        QLayoutItem *item;
        while((item = _ui->runConfigurations->layout()->takeAt(0)) != 0)
        {
            delete item->widget();
            delete item;
        }
    }


    RunConfiguration *runConfiguration = new RunConfiguration(_project, _ui->runConfigurations, runConfig);
    _ui->runConfigurations->layout()->addWidget(runConfiguration);

    // New run configurations should never be added to project
    // Instead, should be added only when ran
//    if(addToProject)
//    {
//        // Add the run configuration to the project
//        //RunConfig *runConfig = runConfiguration->getRunConfig();
//        //_project->addRunConfig(runConfig);
//    }

    connect (runConfiguration, SIGNAL(obtainedResultGraph(QString, RunConfig*)), this, SLOT(handleResultGraph(QString, RunConfig*))  );

    return runConfiguration;
}

}
