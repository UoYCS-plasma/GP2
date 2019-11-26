/*!
 * \file
 */
#include "welcome.hpp"
#include "ui_welcome.h"

#include "project.hpp"

#include <QFile>
#include <QSignalMapper>

namespace Developer {

Welcome::Welcome(QWidget *parent)
    : QWidget(parent)
    , _ui(new Ui::Welcome)
    , _mapper(0)
{
    _ui->setupUi(this);

    // Load the main stylesheet and apply it to this widget
    QFile fp(":/stylesheets/welcome.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);

    _ui->newProjectButton->setMainText(tr("New Project"));
    _ui->newProjectButton->setSubtext(tr("Create a new GP project"));

    _ui->openProjectButton->setMainText(tr("Open Project"));
    _ui->openProjectButton->setSubtext(tr("Open an existing GP project"));
}

Welcome::~Welcome()
{
    delete _ui;
}

void Welcome::recentProjectsUpdated(QStringList projects)
{
    QLayoutItem *item;
    while((item = _ui->recentProjectsGroup->layout()->takeAt(0)) != 0)
    {
        QWidget* widget = item->widget();
        if (_mapper != 0)
            _mapper->removeMappings(widget);
        delete widget;
        delete item;
    }

    if(projects.count() < 1)
    {
        // No recent projects
        _ui->recentProjectsGroup->layout()->addWidget(
                    new QLabel(tr("No recent projects"), _ui->recentProjectsGroup)
                    );
    }
    else
    {
        if(_mapper == 0)
        {
            _mapper = new QSignalMapper(this);
//            qDebug() << "    welcome.cpp: Creating fresh QSignalMapper";
        }

//        qDebug() << "    welcome.cpp: Updating list of projects";

        // Clear the current list
        _mapper->disconnect();

        // There are recent projects, add them to the widget
        for(QStringList::iterator iter = projects.begin();
            iter != projects.end(); ++iter)
        {
            // Attempt to make this into a project, we need to be able to get a
            // name
            QString projectPath = *iter;
            QFileInfo projectName(projectPath);

            StyledButton *button = new StyledButton(_ui->recentProjectsGroup);
            button->setMainText(projectName.fileName());
            button->setSubtext(projectPath);
            connect(button, SIGNAL(pressed()), _mapper, SLOT(map()));
            _mapper->setMapping(button, projectPath);

            _ui->recentProjectsGroup->layout()->addWidget(button);
        }

        connect(_mapper, SIGNAL(mapped(QString)),
                this, SLOT(openProject(QString)));
    }
}

void Welcome::newProject()
{
    emit newProjectClicked();
}

void Welcome::openProject(QString path)
{
    if(path.isEmpty())
        emit openProjectClicked();
    else
        emit openProjectClicked(path);
}

}
