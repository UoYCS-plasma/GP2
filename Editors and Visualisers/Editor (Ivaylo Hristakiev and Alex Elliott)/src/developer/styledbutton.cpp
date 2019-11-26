/*!
 * \file
 */
#include "styledbutton.hpp"
#include "ui_styledbutton.h"

#include <QMouseEvent>

namespace Developer {

StyledButton::StyledButton(QWidget *parent)
    : QWidget(parent)
    , _ui(new Ui::StyledButton)
{
    _ui->setupUi(this);

    setStyleSheet("QLabel#subtextLabel { color: #555; }");
}

StyledButton::~StyledButton()
{
    delete _ui;
}

void StyledButton::setMainText(QString text)
{
    _ui->mainTextLabel->setText(text);
}

void StyledButton::setSubtext(QString text)
{
    _ui->subtextLabel->setText(text);
}

void StyledButton::enterEvent(QEvent *event)
{
    event->accept();
    setStyleSheet("QLabel#subtextLabel { color: #555; } QWidget#buttonBackground { background: #f0f0f0; }");
}

void StyledButton::leaveEvent(QEvent *event)
{
    event->accept();
    setStyleSheet("QLabel#subtextLabel { color: #555; }");
}

void StyledButton::mousePressEvent(QMouseEvent *event)
{
    event->accept();
    emit pressed();
}

}
