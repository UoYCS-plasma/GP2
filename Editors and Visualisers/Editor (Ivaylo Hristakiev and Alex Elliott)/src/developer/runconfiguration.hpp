/*!
 * \file
 */
#ifndef RUNCONFIGURATION_HPP
#define RUNCONFIGURATION_HPP

#include <QWidget>

namespace Ui {
    class RunConfiguration;
}

namespace Developer {

class Project;
class Rule;
class Graph;
class RunConfig;

class RunConfiguration : public QWidget
{
    Q_OBJECT
    
public:
    explicit RunConfiguration(Project *proj, QWidget *parent = 0, RunConfig* config = 0);
    ~RunConfiguration();

    RunConfig *getRunConfig();
    QString name() const;
    QString program() const;
    QString graph() const;

    void setName(QString name);
    void setProgram(QString programName);
    void setGraph(QString graphName);

public slots:
    void toggleDetails();

    void updatePrograms();
    void updateGraphs();
    void runConfiguration();

signals:
    void obtainedResultGraph(QString resultLocation, RunConfig* runConfig);
    
private slots:
    void on_deleteButton_clicked();

private:
    Ui::RunConfiguration *_ui;
    Project *_project;
    bool _existsInProject;
    RunConfig *_config;
    bool run(QString program, QString graph, QString output);
		int call(QString cmd);
    QString rulesToQString(QVector<Rule *> rules);

    int _runs;
};

}

#endif // RUNCONFIGURATION_HPP
