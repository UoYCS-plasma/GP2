/*!
 * \file
 */
#ifndef RESULTS_HPP
#define RESULTS_HPP

#include <QWidget>
#include <QTreeWidgetItem>

namespace Ui {
    class Results;
}

namespace Developer {

// Forward declaration
class Project;
class Graph;
class RunConfig;
class GraphWidget;

class Results : public QWidget
{
    Q_OBJECT
    
public:
    explicit Results(QWidget *parent = 0);
    ~Results();

    /*!
     * \brief Set this widget's project to the provided one and update child
     *  widgets
     * \param project   The project to use
     */
    void setProject(Project *project);    


public slots:
    void handleGraphHasFocus(GraphWidget *graphWidget);
    void handleGraphLostFocus(GraphWidget *graphWidget);

    /*!
     * \brief Slot which handles one of the files being clicked in the tree
     *  widget
     *
     * The first thing this member function does is to determine what kind of
     * element has been clicked. The interesting nodes in the tree have as their
     * parent one of the file types (rule, program, graph). Each of these three
     * is handled separately and any without any of these conditions are
     * discarded.
     *
     * \param item  The tree item which has been clicked
     */
    void graphClicked(QTreeWidgetItem *item);

    /*!
     * \brief Slot to handle adding a result graph. The graph is added to the respective runConfig tree item.
     *
     * The tree is constructed with root nodes for each file type, then the list
     * of files under each type are appended to those nodes. The status of each
     * file affects how it should be displayed.
     */
    void addResultGraph(QString resultLocation, RunConfig* runConfig);

signals:
    void graphHasFocus(GraphWidget *graphWidget);
    void graphLostFocus(GraphWidget *graphWidget);

private:
    Ui::Results *_ui;
    Project *_project;
    QMap<Graph *, QTreeWidgetItem *> _graphMap;
    //Graph *_currentGraph;

    QMap<RunConfig *, QTreeWidgetItem *> _configMap;
    //RunConfig *_currentConfig;
};

}

#endif // RESULTS_HPP
