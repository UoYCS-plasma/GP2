/*!
 * \file
 */
#ifndef STYLEDBUTTON_HPP
#define STYLEDBUTTON_HPP

#include <QWidget>

namespace Ui {
    class StyledButton;
}

namespace Developer {

class StyledButton : public QWidget
{
    Q_OBJECT
    
public:
    explicit StyledButton(QWidget *parent = 0);
    ~StyledButton();

    void setMainText(QString text);
    void setSubtext(QString text);

    void enterEvent(QEvent *event);
    void leaveEvent(QEvent *event);
    void mousePressEvent(QMouseEvent *event);

signals:
    void pressed();
    
private:
    Ui::StyledButton *_ui;
};

}

#endif // STYLEDBUTTON_HPP
