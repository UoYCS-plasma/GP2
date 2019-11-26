/*!
 * \file
 */
#ifndef PRETTYTABWIDGET_HPP
#define PRETTYTABWIDGET_HPP

#include <QWidget>
#include <QMap>
#include <QPair>

namespace Ui {
class PrettyTabWidget;
}

namespace Developer {

/*!
 * \brief The PrettyTabWidget class is a custom replacement for QTabWidget which
 *        provides a "West" positioned set of styled tabs with icons.
 *
 * This is not a complete drop-in replacement for QTabWidget and cannot be
 * moved to another context without alteration due to a lack of generalisation.
 * The reason for this is that QTabWidget is not amenable to icon/button type
 * styled tabs, and a custom approach is (in my view) preferable to pressing a
 * different UI element into service as a tab widget (as is the approach taken
 * by Qxt with a QTableView).
 *
 * If the desktop UI elements in Qt Quick were production-ready at this point it
 * would have made for a compelling alternative option as this type of UI is
 * easy to produce in Qt Quick - however since this is the only element which
 * needed special care I felt this was an easier option as opposed to packaging
 * and depending upon the desktop components in Qt's gitorious.
 *
 * \section usage Usage
 *
 * This widget can be used through promotion of a QWidget in Qt Designer to the
 * PrettyTabWidget (as it is in mainwindow.ui) and can then be interacted with
 * using addTab just like QTabWidget. For example, to create a tab widget with
 * two tabs you could write this code:
 *
 * \code
 *  // Construct the tab widget and some child widgets to place in it
 *  PrettyTabWidget *tabWidget = new PrettyTabWidget(this);
 *  QWidget *widget1 = new QWidget(this);
 *  QWidget *widget2 = new QWidget(this);
 *
 *  // Add both tabs to the tab widget with icons and title text
 *  tabWidget->addTab(
 *      widget1,
 *      QIcon(QPixmap(":/myimages/image1.png")),
 *      tr("First Tab")
 *      );
 *  tabWidget->addTab(
 *      widget2,
 *      QIcon(QPixmap(":/myimages/image2.png")),
 *      tr("Second Tab")
 *      );
 * \endcode
 *
 * This sets up a basic PrettyTabWidget containing both widget1 and widget2 as
 * views.
 *
 * \section menus Menus
 *
 * In addition to the basic functionality discussed above the PrettyTabWidget
 * class can also support the grouping of tabs into distinct menus. When the
 * object is constructed an initial "default" menu is included automatically
 * which allows for simple construction of tab widgets without separation. If
 * that separation is desired however additional menus with headers can be added
 * to the tab widget using addMenu().
 *
 * Each added menu can be targeted using the last parameter of addTab to allow
 * for tabs to be added in any order you wish. If we revisit the above example
 * but instead decide that we want the tabs to appear in different menus then
 * this can be achieved like this:
 *
 * \code
 *  // Construct the tab widget and some child widgets to place in it
 *  // Note: this automatically creates a "default" menu
 *  PrettyTabWidget *tabWidget = new PrettyTabWidget(this);
 *  QWidget *widget1 = new QWidget(this);
 *  QWidget *widget2 = new QWidget(this);
 *
 *  // Add a second menu to the tab widget
 *  tabWidget->addMenu(tr("Other Tabs"));
 *
 *  // Add both tabs to the tab widget with icons and title text
 *  // Start with the second tab, and assume that we want to place this under
 *  // "Other Tabs" rather than the default menu
 *  tabWidget->addTab(
 *      widget2,
 *      QIcon(QPixmap(":/myimages/image2.png")),
 *      tr("Second Tab"),
 *      "Other Tabs"
 *      );
 *  // Now add the first tab, since we omit the last argument it is placed in the
 *  // default menu
 *  tabWidget->addTab(
 *      widget1,
 *      QIcon(QPixmap(":/myimages/image1.png")),
 *      tr("First Tab")
 *      );
 * \endcode
 */
class PrettyTabWidget : public QWidget
{
    Q_OBJECT
    
public:
    /*!
     * \brief Constructs a new PrettyTabWidget with a default menu
     * \param parent This widget's parent widget
     * \sa ~PrettyTabWidget
     */
    explicit PrettyTabWidget(QWidget *parent = 0);

    /*!
     * \brief Destroys the PrettyTabWidget and releases children
     * \sa PrettyTabWidget
     */
    ~PrettyTabWidget();

    /*!
     * \brief Get the current active tab in this widget as a pair of (menu,id)
     * \return A pair consisting of the menu in which the current tab resides
     *  followed by its index in that menu
     */
    QPair<QString, int> currentTab() const;

    /*!
     * \brief Set whether a particular tab should be activated or not
     *
     * This allows for all or none of the tabs to appear as active or inactive
     * by controlling the tabs by hand. In normal behaviour the tab widget will
     * maintain focus on the active tab as expected.
     *
     * \param menu      The menu of the item to activate/deactivate. If the tab
     *                  was not explicitly placed in a menu then it falls under
     *                  "default"
     * \param index     The index of the tab to activate/deactivate. This is
     *                  relative to the menu being targeted
     * \param enabled   Boolean settings whether the tab should become active or
     *                  inactive
     */
    void setTabEnabled(const QString &menu, int index, bool enabled);

    /*!
     * \brief Add a new menu to the PrettyTabWidget
     *
     * Additional menus automatically include a header containing the text
     * passed to create it. This text must be uniquely identifying for the menu
     * addition to be successful, note that the name "default" is already
     * reserved for the initial menu.
     *
     * \param menu  The name and title text of the menu to be created
     * \return  Boolean, true if successfully added, false otherwise.
     */
    bool addMenu(const QString &menu);

    /*!
     * \brief Add a new tab to the PrettyTabWidget
     *
     * When working as a simple drop-in replacement for a QTabWidget the final
     * parameter can be safely ignored and the function used like addTab in a
     * QTabWidget. The provided widget will be added with a corresponding tab
     * made up of the provided icon and label.
     *
     * If you wish to partition tabs into menus then the final parameter allows
     * for you to select other menus (as opposed to the initially created
     * "default" menu) using the string ID given when the menu was added with
     * addMenu().
     *
     * Tabs are appended to the end of the menu they are added to.
     *
     * \param page  The widget which should appear in the viewport when this tab
     *              is active
     * \param icon  The icon for this tab
     * \param label The text to appear on this tab
     * \param menu  Optional. The menu this tab should be added to, if not
     *              present the tab is added to the initial "default" menu.
     * \return
     */
    QPair<QString, int> addTab(QWidget *page, const QIcon &icon, const QString &label, const QString &menu = QString());

signals:
    /*!
     * \brief Signal emitted when the current tab has changed
     * \param title The title of the tab which now has focus in the tab widget
     * \sa setCurrentIndex
     */
    void currentChanged(QString title);

public slots:
    /*!
     * \brief Set the current active tab as if it had been clicked
     *
     * This entails both activating the selected tab and clearing the existing
     * selection to ensure that only the specified tab is active when the
     * function has completed its task.
     *
     * \param menu  The menu of the tab which should be the active tab. If it
     *              was not explicitly placed in a menu it is in "default"
     * \param index The index of the tab relative to the menu it resides in
     */
    void setCurrentIndex(QString menu, int index);

protected slots:
    /*!
     * \brief Recieving slot for the PrettyTab objects sending a signal that
     *  they have been activated (clicked).
     * \param menu  The menu of the tab which has been clicked
     * \param index The index of the tab within the specified menu
     */
    void tabTriggered(QString menu, int index);
    
private:
    Ui::PrettyTabWidget *_ui;
    QPair<QString, int> _currentTab;
    QMap<QPair<QString, int>, int> _pageMapping;
    QMap<QPair<QString, int>, QString> _nameMapping;
};

}

#endif // PRETTYTABWIDGET_HPP
