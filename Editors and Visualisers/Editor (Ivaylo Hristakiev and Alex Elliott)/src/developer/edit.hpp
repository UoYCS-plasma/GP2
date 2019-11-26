#ifndef EDIT_HPP
/*!
 * \file
 */
#define EDIT_HPP

#include <QWidget>
#include <QTreeWidgetItem>

namespace Ui {
    class Edit;
}

namespace Developer {

// Forward declaration
class Project;
class GPFile;
class GraphWidget;

/*!
 * \brief The Edit class provides a container for editing all three file types
 *
 * The files supported by this widget are rules, programs and graphs contained
 * in the currently open project.
 *
 * \todo Investigate extending this to include separate Macro definitions
 */
class Edit : public QWidget
{
    Q_OBJECT
    
public:
    /*!
     * \brief Create a new Edit widget
     *
     * This does not completely set up the widget for use, setProject() should
     * be called before this widget is made active.
     *
     * \param parent This widget's parent widget - the MainWindow
     */
    explicit Edit(QWidget *parent = 0);

    /*!
     * \brief Destroy this widget and free memory
     */
    ~Edit();

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
    void fileClicked(QTreeWidgetItem *item);

    /*!
     * \brief Slot to handle the project's file list changing
     *
     * The tree is constructed with root nodes for each file type, then the list
     * of files under each type are appended to those nodes. The status of each
     * file affects how it should be displayed.
     */
    void fileListChanged();

    void fileStatusChanged(QString path, int status);

    void fileRightClicked();

signals:
    void graphHasFocus(GraphWidget *graphWidget);
    void graphLostFocus(GraphWidget *graphWidget);
    
private:
    Ui::Edit *_ui;
    Project *_project;
    QMap<GPFile *, QTreeWidgetItem *> _treeMap;
    GPFile *_currentFile;
};

}

#endif // EDIT_HPP
