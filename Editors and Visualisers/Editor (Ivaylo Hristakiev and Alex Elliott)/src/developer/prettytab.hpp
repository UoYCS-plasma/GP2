/*!
 * \file
 */
#ifndef PRETTYTAB_HPP
#define PRETTYTAB_HPP

#include <QWidget>
#include <QString>
#include <QIcon>

namespace Developer {

/*!
 * \brief The PrettyTab class represents one tab in the PrettyTabBar
 *
 * This is a UI element which is primarily involved with drawing itself onto
 * the screen within its custom paintEvent(). It also handles user interaction
 * such as mouse-over, mouse-out and mouse clicks.
 *
 * The tab is either in a "selected" or "unselected" state, it becomes
 * "selected" when it is clicked and emits a pressed() signal. It becomes
 * "unselected" when informed that another tab has focus via its
 * clearSelection() method.
 *
 * The setTop() method is a hack to prevent a double-border situation which
 * occurs because the top item is adjacent to the title bar which has the
 * responsibility for its own bottom border. The top tab should not add a second
 * 1px border, and should skip that part of its paint event.
 *
 * Since this is a simplistic implementation for this particular situation the
 * logic used in the custom paint event is not very customisable. It is
 * currently a hard-coded requirement that the tab be rendered as a vertical
 * stack with the icon appearing above the label, and that it is in an "East"
 * placement with the dividing line between the tabs and the viewport appearing
 * on the right edge of the tab.
 *
 * In addition several internal variables are simply assigned in the initialiser
 * list. Adding complete encapsulation for these member variables would make
 * these widgets more customisable, but it is possible that they should draw
 * this information directly from their parent really, as this would help to
 * ensure consistency is maintained across the whole tab bar.
 *
 * Currently hardcoded members:
 *
 * \code
 *  - Icon size (32x32)
 *      - _iconHeight = 32
 *      - _iconWidth = 32
 *  - Margins
 *      - _topMargin = 6
 *      - _rightMargin = 12
 *      - _bottomMargin = 6
 *      - _leftMargin = 6
 *  - Spacing between icon and label
 *      - _itemSpacing = 6
 * \endcode
 */
class PrettyTab : public QWidget
{
    Q_OBJECT
    
public:
    /*!
     * \brief Construct a new tab with the given icon and label
     * \param label     The text which should appear on this tab
     * \param icon      The icon which should appear on this tab
     * \param parent    This widget's parent widget
     */
    explicit PrettyTab(const QString &label, const QIcon &icon, QWidget *parent = 0);
    ~PrettyTab();

    void setEnabled(bool enabled);
    void setSelected(bool selected);
    void setTop(bool top);
    void clearSelection();

protected:
    void enterEvent(QEvent *event);
    void leaveEvent(QEvent *event);
    void mousePressEvent(QMouseEvent *event);
    void paintEvent(QPaintEvent *event);

    QSize sizeHint() const;

signals:
    void pressed();
    
private:
    QString _label;
    QIcon _icon;

    int _iconHeight;
    int _iconWidth;

    int _topMargin;
    int _rightMargin;
    int _bottomMargin;
    int _leftMargin;
    int _itemSpacing;

    bool _enabled;
    bool _mouseOver;
    bool _active;
    bool _top;
};

}

#endif // PRETTYTAB_HPP
