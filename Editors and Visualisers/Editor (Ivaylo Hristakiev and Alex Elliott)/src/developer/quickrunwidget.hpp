/*!
 * \file
 */
#ifndef QUICKRUNWIDGET_HPP
#define QUICKRUNWIDGET_HPP

#include <QWidget>

namespace Ui {
class QuickRunWidget;
}

namespace Developer {

class Project;

/*!
 * \brief The QuickRunWidget class allows the user to quickly run a saved run
 *  configuration
 *
 * This class sits in the top right of the MainWindow and allow the user to
 * select one of their defined run configurations from a combo box and then run
 * them using an adjacent button.
 */
class QuickRunWidget : public QWidget
{
    Q_OBJECT
    
public:
    /*!
     * \brief Create a new QuickRunWidget
     * \param parent This widget's parent widget (MainWindow)
     */
    explicit QuickRunWidget(QWidget *parent = 0);
    /*!
     * \brief Destroy this QuickRunWidget and free memory
     */
    ~QuickRunWidget();

    /*!
     * \brief Set the currently open project to the provided project
     * \param project   The new project which run configurations will be drawn
     *  from
     */
    void setProject(Project *project);

public slots:
    /*!
     * \brief Slot which handles the user pressing the "run" button
     *
     * Whichever run configuration currently appears in the widget's combo box
     * should be run.
     */
    void run();
    
private:
    Ui::QuickRunWidget *_ui;
    Project *_project;
};

}

#endif // QUICKRUNWIDGET_HPP
