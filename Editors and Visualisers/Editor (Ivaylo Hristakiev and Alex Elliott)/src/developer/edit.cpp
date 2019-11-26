/*!
 * \file
 */
#include "edit.hpp"
#include "ui_edit.h"

#include "helpdialog.hpp"
#include "project.hpp"

#include <QMenu>
#include <QPoint>

namespace Developer {

Edit::Edit(QWidget *parent)
    : QWidget(parent)
    , _ui(new Ui::Edit)
    , _project(0)
    , _currentFile(0)
{
    _ui->setupUi(this);

    // Load the main stylesheet and apply it to this widget
    QFile fp(":/stylesheets/main.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);

    connect(_ui->graphEdit, SIGNAL(graphHasFocus(GraphWidget*)),
            this, SLOT(handleGraphHasFocus(GraphWidget*)));
    connect(_ui->graphEdit, SIGNAL(graphLostFocus(GraphWidget*)),
            this, SLOT(handleGraphLostFocus(GraphWidget*)));
}

Edit::~Edit()
{
    delete _ui;
}

void Edit::setProject(Project *project)
{
    if(project == 0)
    {
        qDebug() << "Edit::setProject() handed a null pointer... not pleased";
        return;
    }

    _project = project;
    _currentFile = 0;

    if ((!_ui) || (!_ui->stackedWidget) || (!_ui->ruleEdit) || (!_ui->graphEdit)  )
    {
        qDebug() << "Error: Edit UI not initialized; ignoring";
        return;
    }

    // Set up rule edit
    if(_project->rules().count() > 0)
    {
        Rule *rule = _project->rules().at(0);
        _ui->ruleEdit->setRule(rule);
        if(_ui->stackedWidget->currentIndex() == 0)
        {
            _project->setCurrentFile(rule->absolutePath(), Project::RuleFile);
            _currentFile = rule;
        }
    }
    else
        _ui->ruleEdit->setVisible(false);

    // Set up program edit
    if(_project->programs().count() > 0)
    {
        Program *prog = _project->programs().at(0);
        _ui->programEdit->setProgram(prog);
        if(_ui->stackedWidget->currentIndex() == 1)
        {
            _project->setCurrentFile(prog->absolutePath(), Project::ProgramFile);
            _currentFile = prog;
        }
    }
    else
        _ui->graphEdit->setVisible(false);

    // Set up graph edit
    if(_project->graphs().count() > 0)
    {
        Graph *graph = _project->graphs().at(0);
        _ui->graphEdit->setGraph(graph);
        if(_ui->stackedWidget->currentIndex() == 2)
        {
            _project->setCurrentFile(graph->absolutePath(), Project::GraphFile);
            _currentFile = graph;
        }
    }
    else
        _ui->graphEdit->setVisible(false);

    connect(_project, SIGNAL(fileListChanged()), this, SLOT(fileListChanged()));
    connect(_project, SIGNAL(fileStatusChanged(QString,int)),
            this, SLOT(fileStatusChanged(QString,int))
            );
    fileListChanged();
}

void Edit::fileClicked(QTreeWidgetItem *item)
{
    QTreeWidgetItem *parent = item->parent();
    if(parent == 0)
        return;

    // Handle a clicked rule
    if(parent->text(0) == tr("Rules"))
    {
        Rule *rule = _project->rule(item->text(0));
        if(rule == 0)
        {
            qDebug() << "Edit::fileClicked() could not find rule: "
                     << item->text(0);
            return;
        }
        _ui->ruleEdit->setVisible(true);
        _ui->stackedWidget->setCurrentIndex(0);
        _ui->ruleEdit->setRule(rule);
        _currentFile = rule;
        _project->setCurrentFile(item->text(0), Project::RuleFile);
    }

    // Handle a clicked program
    if(parent->text(0) == tr("Programs"))
    {
        Program *prog = _project->program(item->text(0));
        if(prog == 0)
        {
            qDebug() << "Edit::fileClicked() could not find program: "
                     << item->text(0);
            return;
        }
        _ui->programEdit->setVisible(true);
        _ui->stackedWidget->setCurrentIndex(1);
        _ui->programEdit->setProgram(prog);
        _currentFile = prog;
        _project->setCurrentFile(item->text(0), Project::ProgramFile);
    }

    // Handle a clicked graph
    if(parent->text(0) == tr("Graphs"))
    {
        Graph *graph = _project->graph(item->text(0));
        if(graph == 0)
        {
            qDebug() << "Edit::fileClicked() could not find graph: "
                     << item->text(0);
            return;
        }
        _ui->graphEdit->setVisible(true);
        _ui->stackedWidget->setCurrentIndex(2);
        _ui->graphEdit->setGraph(graph);
        _currentFile = graph;
        _project->setCurrentFile(item->text(0), Project::GraphFile);
    }
}

void Edit::fileListChanged()
{
    // Clear the tree of existing items
    _ui->projectTreeWidget->clear();
    _treeMap.clear();

    // Add the root node with the project name
    QStringList items;
    items << _project->name();
    QTreeWidgetItem *root = new QTreeWidgetItem(items);
    root->setIcon(0, QIcon(QPixmap(":/icons/application-icon.png")));
    _ui->projectTreeWidget->addTopLevelItem(root);

    // Loop over rules adding each to the list
    items.clear(); items << tr("Rules");
    QTreeWidgetItem *rules = new QTreeWidgetItem(items);
    rules->setIcon(0, QIcon(QPixmap(":/icons/small_folder_brick.png")));
    root->addChild(rules);

    QVector<Rule *> projectRules = _project->rules();
    for(QVector<Rule *>::iterator iter = projectRules.begin();
        iter != projectRules.end(); ++iter)
    {
        Rule *r = *iter;
        items.clear(); items << r->name();
        QTreeWidgetItem *item = new QTreeWidgetItem(items);
        item->setToolTip(0, r->absolutePath());
        switch(r->status())
        {
        case GPFile::Modified:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_page_save.png")));
            break;
        case GPFile::ExternallyModified:
        case GPFile::Error:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_page_error.png")));
            break;
        case GPFile::Deleted:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_page_delete.png")));
            break;
        case GPFile::ReadOnly:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_lock.png")));
            break;
        case GPFile::Normal:
            item->setIcon(0, QIcon());
            break;
        default:
            // Do nothing
            break;
        }
        rules->addChild(item);
        _treeMap.insert(r, item);
    }

    // Loop over programs adding each to the list
    items.clear(); items << tr("Programs");
    QTreeWidgetItem *programs = new QTreeWidgetItem(items);
    programs->setIcon(0, QIcon(QPixmap(":/icons/small_folder_page.png")));
    root->addChild(programs);

    QVector<Program *> projectPrograms = _project->programs();
    for(QVector<Program *>::iterator iter = projectPrograms.begin();
        iter != projectPrograms.end(); ++iter)
    {
        Program *p = *iter;
        items.clear(); items << p->name();
        QTreeWidgetItem *item = new QTreeWidgetItem(items);
        item->setToolTip(0, p->absolutePath());
        switch(p->status())
        {
        case GPFile::Modified:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_page_save.png")));
            break;
        case GPFile::ExternallyModified:
        case GPFile::Error:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_page_error.png")));
            break;
        case GPFile::Deleted:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_page_delete.png")));
            break;
        case GPFile::ReadOnly:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_lock.png")));
            break;
        case GPFile::Normal:
            item->setIcon(0, QIcon());
            break;
        default:
            // Do nothing
            break;
        }

        // Create right-click menu
//        QMenu* menu = new QMenu();
//        QAction* open = new QAction(tr("Open A File"), menu);
//        connect(open, SIGNAL(triggered()), this, SLOT(fileRightClicked()));

//        _ui->projectTreeWidget->setContextMenuPolicy(Qt::ActionsContextMenu);
//        _ui->projectTreeWidget->addAction(open);

        // Append item to parent tree
        programs->addChild(item);
        _treeMap.insert(p, item);
    }

    // Loop over graphs adding each to the list
    items.clear(); items << tr("Graphs");
    QTreeWidgetItem *graphs = new QTreeWidgetItem(items);
    graphs->setIcon(0, QIcon(QPixmap(":/icons/small_folder.png")));
    root->addChild(graphs);

    QVector<Graph *> projectGraphs = _project->graphs();
    for(QVector<Graph *>::iterator iter = projectGraphs.begin();
        iter != projectGraphs.end(); ++iter)
    {
        Graph *g = *iter;
        items.clear(); items << g->fileName();
        QTreeWidgetItem *item = new QTreeWidgetItem(items);
        item->setToolTip(0, g->absolutePath());
        switch(g->status())
        {
        case GPFile::Modified:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_page_save.png")));
            break;
        case GPFile::ExternallyModified:
        case GPFile::Error:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_page_error.png")));
            break;
        case GPFile::Deleted:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_page_delete.png")));
            break;
        case GPFile::ReadOnly:
            item->setIcon(0, QIcon(QPixmap(":/icons/small_lock.png")));
            break;
        case GPFile::Normal:
            item->setIcon(0, QIcon());
            break;
        default:
            // Do nothing
            break;
        }
        graphs->addChild(item);
        _treeMap.insert(g, item);
    }

    _ui->projectTreeWidget->expandAll();
}

void Edit::fileRightClicked()
{
    //qDebug() << "Right click happened.";
    QAction *action = static_cast<QAction *>(sender());
    if (!action)
    {
        qDebug() << "  edit.cpp: Could not find the clicked Action.";
        return;
    }

    QMenu* menu = static_cast<QMenu *>(action->parent());
    if (!menu)
    {
        qDebug() << "  edit.cpp: Action does not have a parent.";
        return;
    }

    //qDebug() << "  edit.cpp: Success";

}

void Edit::fileStatusChanged(QString path, int status)
{
    GPFile *file = _project->file(path);

    if(file == 0)
    {
        qDebug() << "Edit::fileStatusChanged() could not locate the file with "
                 << "the provided path: " << path;
        return;
    }

    if(!_treeMap.contains(file))
    {
        qDebug() << "Edit::fileStatusChanged called but could not locate file.";
        qDebug() << "path = " << path;
        qDebug() << "status = " << status;
        return;
    }

    QTreeWidgetItem *item = _treeMap[file];
    if(item == 0)
        return;

    switch(status)
    {
    case GPFile::Modified:
        item->setIcon(0, QIcon(QPixmap(":/icons/small_page_save.png")));
        break;
    case GPFile::ExternallyModified:
    case GPFile::Error:
        item->setIcon(0, QIcon(QPixmap(":/icons/small_page_error.png")));
        break;
    case GPFile::Deleted:
        item->setIcon(0, QIcon(QPixmap(":/icons/small_page_delete.png")));
        break;
    case GPFile::ReadOnly:
        item->setIcon(0, QIcon(QPixmap(":/icons/small_lock.png")));
        break;
    case GPFile::Normal:
        item->setIcon(0, QIcon());
        break;
    default:
        qDebug() << "Unhandled status: " << status;
        // Do nothing
        break;
    }
}

void Edit::handleGraphHasFocus(GraphWidget *graphWidget)
{
    emit graphHasFocus(graphWidget);
}

void Edit::handleGraphLostFocus(GraphWidget *graphWidget)
{
    emit graphLostFocus(graphWidget);
}

}
