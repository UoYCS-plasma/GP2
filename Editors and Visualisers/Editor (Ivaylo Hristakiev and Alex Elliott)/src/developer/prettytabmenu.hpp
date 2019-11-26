/*!
 * \file
 */
#ifndef PRETTYTABMENU_HPP
#define PRETTYTABMENU_HPP

#include <QVBoxLayout>
#include <QSignalMapper>
#include <QVector>

#include "prettytab.hpp"

namespace Developer {

/*!
 * \brief The PrettyTabMenu class represents each individual menu making up the
 *        whole of the PrettyTabBar
 *
 * The structure of the PrettyTabMenu is an optional title followed by a
 * sequence of tabs. A title is expected to be provided for every menu apart
 * from the first menu constructed internally (the "default" menu) - and this
 * is reflected in that any tab menu constructed without a title will return
 * "default" in title() and will send "default" in the tabTriggered() signal.
 *
 * To add a tab to the menu you would call addTab() and make note of the integer
 * returned, this is the integer which will be reported by the tabTriggered()
 * signal along with the title of the menu.
 *
 * Since another tab menu can take focus from one of the tabs in this menu the
 * menu provides a clearSelection() method which should be called whenever
 * a tab in another menu takes focus to ensure that only one tab is "active" at
 * once.
 */
class PrettyTabMenu : public QWidget
{
    Q_OBJECT
public:
    explicit PrettyTabMenu(const QString &title = QString(), QWidget *parent = 0);

    PrettyTab *tab(int index) const;
    int addTab(const QString &name, const QIcon &icon);

    QString title() const;

    bool contains(int index) const;
    
signals:
    void tabTriggered(QString title, int index);
    
public slots:
    void clearSelection();
    void setCurrentIndex(int index);
    void tabTriggered(int index);
    
private:
    QVBoxLayout *_layout;
    QSignalMapper *_mapper;
    QString _title;
    int _tabCount;
    QVector<PrettyTab *> _tabs;
};

}

#endif // PRETTYTABMENU_HPP
