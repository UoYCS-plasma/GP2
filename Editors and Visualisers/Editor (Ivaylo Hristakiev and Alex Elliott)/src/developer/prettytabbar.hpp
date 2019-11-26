/*!
 * \file
 */
#ifndef PRETTYTABBAR_HPP
#define PRETTYTABBAR_HPP

#include <QMap>

#include "prettytabmenu.hpp"

namespace Developer {

/*!
 * \brief The PrettyTabBar class is a subclass of PrettyTabWidget which follows
 *        the established pattern of QTabWidget -> QTabBar
 *
 * This class provides a "pretty" button-based tab bar for use as a vertically
 * aligned bar connected to a QStackedWidget. It is not fully generalised and as
 * such will not work without modification if taken out of context.
 *
 * The tab bar can be organised into sub-menus, there is a default menu which
 * will appear first, followed by any user-defined sub-menus. For example:
 *
 * \code
 * (default):
 *  - Tab1
 *  - Tab2
 * Other Menu:
 *  - Tab3
 * \endcode
 *
 * This class is intended to be used primarily as a component of PrettyTabWidget
 * but could be used elsewhere with some modification in a similar way to
 * QTabBar in vanilla Qt.
 */
class PrettyTabBar : public QWidget
{
    Q_OBJECT

public:
    /*!
     * \brief Creates a new PrettyTabBar with an initial "default" menu
     * \param parent    This widget's parent widget
     */
    explicit PrettyTabBar(QWidget *parent = 0);

    /*!
     * \brief Returns the tab present at the index specified
     *
     * Locates the tab at the specified index relative to the menu in which it
     * exists. If a tab cannot be found then the pointer returned is 0.
     *
     * \param menuName  The menu in which this tab resides
     * \param index     The index of this tab relative to the menu in which this
     *                  tab resides
     * \return The tab at the specified menu, at the specified index if one
     *  exists. If none exists then the function returns 0.
     * \sa addTab containsMenu
     */
    PrettyTab *tab(const QString &menuName, int index) const;

    /*!
     * \brief Add a menu with the specified name and title text to the tab bar
     *
     * The name must be unique, noting that the name "default" is reserved
     * automatically in the constructor for the initial menu.
     *
     * \param name  The name and title text of the new menu
     * \return Boolean, true if successfully added, false otherwise
     */
    bool addMenu(const QString &name);

    /*!
     * \brief Add a new tab to the menu
     *
     * When used as a simple QTabBar replacement the final parameter can be
     * safely ignored and a tab will simply be added with the provided label and
     * icon. If additional menus have been added to the tab bar then the final
     * parameter controls which of the menus the inserted tab is added to.
     *
     * Tabs are appended to the end of the menus they are added to.
     *
     * \param name  The label text for this new tab
     * \param icon  The icon which should appear on this new tab
     * \param menu  Optional. The menu which should contain this tab. If none is
     *              provided then the tab is added to the "default" menu
     * \return A pair consisting of the name of the menu the tab resides in and
     *  its index relative to this menu. This is sufficient to uniquely identify
     *  it
     *  \sa containsMenu
     */
    QPair<QString, int> addTab(const QString &name, const QIcon &icon, const QString &menu = QString());

    /*!
     * \brief Boolean test for whether a menu of the given name exists in the
     *  tab bar or not
     * \param menu  The name of the menu to test for
     * \return Boolean, true if a menu with this name exists, false otherwise
     */
    bool containsMenu(QString menu) const;

    void clearSelection();
    
signals:
    /*!
     * \brief Signal emitted when the current active tab has changed
     * \param menu  The name of the menu in which the new active tab exists
     * \param index The index of the new active tab relative to the menu it is in
     * \sa setCurrentIndex tabTriggered
     */
    void currentChanged(QString menu, int index);
    
public slots:
    /*!
     * \brief Set the currently active tab to the specified tab
     *
     * If a non-existant tab is provided then this function returns, doing
     * nothing.
     *
     * \param menu  The menu in which the tab to switch to resides
     * \param index The index of the tab to switch to relative to the menu it is
     *  in
     * \sa currentChanged
     */
    void setCurrentIndex(QString menu, int index);

protected slots:
    /*!
     * \brief Internal slot attached to PrettyTab to catch when a tab is
     *  activated/clicked
     *
     * PrettyTabBar
     *
     * \param menu  The menu in which the activated tab resides
     * \param index The index of the activated tab relative to the menu it is in
     * \sa PrettyTabMenu::tabTriggered()
     */
    void tabTriggered(QString menu, int index);

private:
    QVBoxLayout *_layout;
    QMap<QString, PrettyTabMenu *> _menus;
    int _menuCount;
};

}

#endif // PRETTYTABBAR_HPP
