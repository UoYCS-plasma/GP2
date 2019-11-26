/*!
 * \file
 */
#ifndef PRETTYTABHEADER_HPP
#define PRETTYTABHEADER_HPP

#include <QWidget>

namespace Ui {
    class PrettyTabHeader;
}

namespace Developer {

/*!
 * \brief The PrettyTabHeader class is a simple form class which represents the
 *        PrettyTabBar's menu headings
 *
 * It is essentially just a QLabel contained within a pair of widgets to provide
 * the correct margins and background.
 */
class PrettyTabHeader : public QWidget
{
    Q_OBJECT
    
public:
    explicit PrettyTabHeader(const QString &title, QWidget *parent = 0);
    ~PrettyTabHeader();
    
private:
    Ui::PrettyTabHeader *_ui;
};

}

#endif // PRETTYTABHEADER_HPP
