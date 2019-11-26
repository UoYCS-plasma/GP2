/*!
 * \file
 */
#include "results.hpp"
#include "ui_results.h"

#include "helpdialog.hpp"
#include "project.hpp"

namespace Developer {

Results::Results(QWidget *parent)
    : QWidget(parent)
    , _ui(new Ui::Results)
{
    _ui->setupUi(this);


    QStringList items;
    items << "Results";
    QTreeWidgetItem *root = new QTreeWidgetItem(items);
    root->setIcon(0, QIcon(QPixmap(":/icons/application-icon.png")));
    _ui->resultsTreeWidget->addTopLevelItem(root);


    connect(_ui->graphEdit, SIGNAL(graphHasFocus(GraphWidget*)),
            this, SLOT(handleGraphHasFocus(GraphWidget*)));
    connect(_ui->graphEdit, SIGNAL(graphLostFocus(GraphWidget*)),
            this, SLOT(handleGraphLostFocus(GraphWidget*)));
}

Results::~Results()
{
    delete _ui;
}

void Results::setProject(Project *project)
{
    if(project == 0)
    {
        qDebug() << "Results::setProject() handed a null pointer... not pleased";
        return;
    }

    _project = project;

    // Clear the existing result graphs
    _ui->resultsTreeWidget->clear();

    QStringList items;
    items << "Results";
    QTreeWidgetItem *root = new QTreeWidgetItem(items);
    root->setIcon(0, QIcon(QPixmap(":/icons/application-icon.png")));
    _ui->resultsTreeWidget->addTopLevelItem(root);

    //_currentGraph = 0;

		/* Set up run configurations
		if(_project->runConfigurations().count() > 0)
		{
			RunConfig *config = _project->runConfigurations().at(0);
			_ui->results->setConfig(config);
      if(_ui->stackedWidget->currentIndex() == 0)
      {
          _project->setCurrentFile(config->absolutePath(), Project::RuleFile);
          _currentConfig = config;
      }


		}

    connect(_project, SIGNAL(graphListChanged()), this, SLOT(graphListChanged()));
    connect(_project, SIGNAL(graphStatusChanged(QString,int)),
            this, SLOT(graphStatusChanged(QString,int))
            );
    graphListChanged();*/
}

void Results::graphClicked(QTreeWidgetItem *item)
{
    QTreeWidgetItem *parent = item->parent();
    if(parent == 0)
        return;

    // Handle a clicked result

    Graph* graph = _graphMap.key(item);    // Slow search, maps are optimized for fast retrieval by key, not value
    if (graph == 0)
    {
        qDebug() << "Could not find a corresponding Graph: " << item->text(0);
        return;
    }

    // qDebug() << "Attempting to view Result: " << graph->fileName();

    //graph->open();

    _ui->graphEdit->setEnabled(true);
    _ui->graphEdit->setGraph(graph);
}

void Results::addResultGraph(QString resultLocation, RunConfig* runConfig)
{
    // Check if config exists already in the tree structure
    // If no, create it
    QTreeWidgetItem* config;
    QTreeWidgetItem* graph;
    QStringList items;
    if (!_configMap.contains(runConfig))
    {
        qDebug() << "Adding a new run configuration to Results View: " << runConfig->name();

        items << runConfig->name();
        config = new QTreeWidgetItem(items);

        config->setIcon(0, QIcon(QPixmap(":/icons/small_folder.png")));
        _ui->resultsTreeWidget->addTopLevelItem(config);

        _configMap.insert(runConfig, config);
    }
    else
    {
        config = _configMap[runConfig];
    }  

    if (config == 0)
    {
        qDebug() << "Unable to add result config to Result View " << runConfig->name();    
        return;
    } 

    // Update widget display name
    if (config->text(0) != runConfig->name())
        config->setText(0, runConfig->name());

    // Check if result graph exists in tree structure
    // If no, create it
    Graph* resultGraph = new Graph(resultLocation, this);

    items.clear(); items << resultGraph->fileName();
    graph = new QTreeWidgetItem(items);
    graph->setToolTip(0, resultGraph->absolutePath());

    config->addChild(graph);
    _graphMap.insert(resultGraph, graph);

    _ui->resultsTreeWidget->expandItem(config);
}

void Results::handleGraphHasFocus(GraphWidget *graphWidget)
{
    emit graphHasFocus(graphWidget);
}

void Results::handleGraphLostFocus(GraphWidget *graphWidget)
{
    emit graphLostFocus(graphWidget);
}

}
