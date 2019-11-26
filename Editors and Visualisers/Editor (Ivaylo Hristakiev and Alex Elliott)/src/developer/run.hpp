/*!
 * \file
 */
#ifndef RUN_HPP
#define RUN_HPP

#include <QWidget>
#include "../libgp/errors.h"

namespace Ui {
    class Run;
}

namespace Developer {

class Project;
class Graph;
class RunConfig;
class RunConfiguration;

class Run : public QWidget
{
    Q_OBJECT
    
public:
    explicit Run(QWidget *parent = 0);
    ~Run();

    Project *project() const;

    void setProject(Project *proj);

signals:
    void obtainedResultGraph(QString resultLocation, RunConfig* runConfig);

public slots:
    RunConfiguration *addRunConfiguration(RunConfig* runConfig = 0);
    void handleResultGraph(QString resultLocation, RunConfig* runConfig);
    void handleRunConfigListChanged();
    
private:
    Ui::Run *_ui;
    Project *_project;
    bool _initial;
};

}

#endif // RUN_HPP
