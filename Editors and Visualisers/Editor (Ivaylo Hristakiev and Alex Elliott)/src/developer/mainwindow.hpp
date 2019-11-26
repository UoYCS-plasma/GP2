/*!
 * \file
 */
#ifndef MAINWINDOW_HPP
#define MAINWINDOW_HPP

#include <QMainWindow>
#include <QSignalMapper>

#include "project.hpp"

namespace Ui {
    class MainWindow;
}

class QUndoStack;

namespace Developer {

// Some forward declarations of our main views
class Edit;
class Run;
class Results;
class GraphWidget;

/*!
 * \brief The MainWindow class forms the basis of GP Developer, containing the
 *  primary views
 *
 * The MainWindow is a typical desktop-style application, it consists of a menu
 * bar, a header (which contains the quick run widget), a set of tabs along the
 * left edge (west positioned), a stacked widget containing the main display
 * widgets (welcome, edit, run configurations, results), and a status bar.
 *
 * \code
 * +---------------------------------------------------------+
 * | File | Edit | Tools | Help                              | <- menu bar
 * +---------------------------------------------------------+
 * | [logo]                               +----------------+ |
 * | [logo] GP Developer                  | Quick Run      | | <- title bar
 * | [logo]                               +----------------+ |
 * +---------------------------------------------------------+
 * |--Welcome--|          Welcome to GP Developer            |
 * |    Edit   |                                             | <- tab widget
 * |    Run    |  Text... more stuff, text about the app     |
 * |  Results  |  and more would follow, like this text.     |
 * +---------------------------------------------------------+
 * | Ready.                                                  | <- status bar
 * +---------------------------------------------------------+
 * \endcode
 */
class MainWindow : public QMainWindow
{
    Q_OBJECT
    
public:
    /*!
     * \brief Construct a new MainWindow ready to start the main event loop
     * \param parent Unused, this is a MainWindow and thus has no parent, but it
     *  is part of QMainWindow to accept this
     */
    explicit MainWindow(QWidget *parent = 0);
    /*!
     * \brief Destroy the main window and free memory
     */
    ~MainWindow();

public:
    /*!
     * \brief Get the list of recent projects for the application
     * \return
     */
    QStringList recentProjects() const;

public slots:
    /*!
     * \brief Read in the list of recent projects and update the UI to reflect
     *  this list.
     */
    void updateRecentProjects();

    /*!
     * \brief Add the provided project to the set of recent projects
     *
     * If the project already appears in the list of recent projects then it is
     * just moved to the top of the list.
     *
     * \param project   The path to the project file to add
     */
    void addRecentProject(QString project);

    /*!
     * \brief Set a new project to use within the application and distribute
     *  it to child widgets
     *
     * The main responsibility of this member function is to call setProject()
     * on all of the child widgets which directly use the project to control
     * their state. When setProject() is called the current project is typically
     * about to be deleted, so it is imperative that existing references are
     * updated to prevent addressing released memory.
     *
     * \param project   The project to use and distribute amongst children
     */
    void setProject(Project *project);

    /*!
     * \brief Make widgets requiring a project active and visible
     *
     * Initially the application opens in a state where no project is open and
     * therefore many of the widgets (such as the Edit pane or the Quick Run
     * widget in the header) cannot be meaningfully employed. These widgets are
     * either rendered hidden or made inactive and become active when this
     * member function is called.
     *
     * The function accepts a boolean in case it becomes useful later to allow
     * for the program to return to this state.
     *
     * \param state Determines whether project-requiring widgets should appear
     *      or not, true for yes and false for no.
     */
    void setProjectActive(bool state);

    /*!
     * \brief This slot responds to a user request to create a new project
     *
     * Internally this means presenting the user with the NewProjectWizard to
     * facilitate the process of creating a new project for the user.
     */
    void newProject();

    /*!
     * \brief This slot handles a user request to create a new graph and add it
     *  to the current project
     */
    void newGraph();

    /*!
     * \brief This slot handles a user request to create a new program and add
     *  it to the current project
     */
    void newProgram();

    /*!
     * \brief This slot handles a user request to create a new rule and add it
     *  to the current project
     */
    void newRule();

    /*!
     * \brief This slot responds to a user request to open an existing project
     *
     * This function can be called with a QString path or with no arguments. If
     * no arguments are provided then a QFileDialog is used to select a suitable
     * project file, if a path is provided then that is used to open a project
     * if the file referenced exists.
     *
     * A file must be able to create a valid Project object before the system
     * will make this project the new active project for the application.
     *
     * \param path Optional. A path to an existing project file
     */
    void openProject(QString path = QString());

    void openProgram();
    void openRule();
    void openGraph();

    void closeProject();

    /*!
     * \brief Slot which handles a request to save the current file
     */
    void save();
    /*!
     * \brief Slot which handles a request to save the current file under a new
     *  name
     */
    void saveAs();
    /*!
     * \brief Slot which handles a request to save all files in the project
     */
    void saveAll();

    void setUndoAvailable(bool available);
    void undo();
    void setRedoAvailable(bool available);
    void redo();

    void setTextEditing(bool editing);
    void setTextSelected(bool selected);
    void cut();
    void copy();
    void paste();
    void selectAll();
    void findReplaceCurrentFile();
    void findReplaceProject();

    void layoutTreeTopToBottom();
    void layoutTreeRightToLeft();
    void layoutTreeBottomToTop();
    void layoutTreeLeftToRight();
    void layoutSugiyama();
    void layoutRadialTree();
    void layoutFPP();
    void layoutPlanarDraw();
    void layoutPlanarStraight();
    void layoutSchnyder();
    void layoutPlanarizationGrid();
    void layoutCircular();
    void layoutSpring();
    void layoutDavidsonHarel();
    void layoutFMMM();
    void layoutGEM();

    void exportGraphToPng();
    void exportGraphToSvg();
    void exportGraphToDot();
    void exportGraphToGxl();
    void exportGraphToLaTeX();

    /*!
     * \brief Handle a QWidget close event
     *
     * This reimplements the QWidget closeEvent member function in order to
     * save the window's dimensions, position, and maximisation state.
     *
     * \param event The event which has triggered the handler
     */
    void closeEvent(QCloseEvent *event);

    /*!
     * \brief Show the preferences dialog when requested
     */
    void showPreferences();

    void showFirstRunDialog();

    /*!
     * \brief Show this application's help dialog
     *
     * \todo Extend this signature to allow for specific pages as used in the
     *  Edit widget
     */
    void showApplicationHelp();

    /*!
     * \brief Show a simple dialog with information about the software and icons
     *  used within the application
     *
     * This means that the application conforms with the FatCow icon set's
     * attribution license.
     */
    void showApplicationAbout();

    /*!
     * \brief This slot informs the MainWindow object that something in the
     *  project has changed
     *
     * This means that there will now be unsaved changes, these will persist
     * until all of the files marked as Modified are saved. On each save the
     * MainWindow will query the project to see if all files are now in the
     * "Normal" FileStatus.
     */
    void projectChanged();

    void currentFileChanged(GPFile *f);

    void graphHasFocus(GraphWidget *graphWidget);
    void graphLostFocus(GraphWidget *graphWidget);

signals:
    /*!
     * \brief Signal emitted when the list of recent projects has changed
     */
    void recentProjectsChanged(QStringList projects);

protected:
    /*!
     * \brief Read the last set of window dimensions from settings and apply
     *  them to maintain consistency
     *
     * If the window was maximised last time the program was open then it is
     * simply maximised again and window dimensions/position are not applied.
     */
    void restoreWindowDimensions();

private:
    Ui::MainWindow *_ui;
    Project *_activeProject;
    QStringList _recentProjects;
    QSignalMapper *_mapper;
    GraphWidget *_currentGraph;
    QUndoStack *_undoStack;

    Edit *_edit;
    Run *_run;
    Results *_results;
};

}

#endif // MAINWINDOW_HPP
