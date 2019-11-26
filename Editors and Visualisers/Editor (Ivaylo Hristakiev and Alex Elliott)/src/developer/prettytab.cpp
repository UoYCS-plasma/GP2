/*!
 * \file
 */
#include "prettytab.hpp"

#include <QApplication>
#include <QPainter>
#include <QPaintEvent>
#include <QMouseEvent>
#include <QDebug>

namespace Developer {

PrettyTab::PrettyTab(const QString &label, const QIcon &icon, QWidget *parent)
    : QWidget(parent)
    , _label(label)
    , _icon(icon)
    , _iconHeight(32)
    , _iconWidth(32)
    , _topMargin(6)
    , _rightMargin(12)
    , _bottomMargin(6)
    , _leftMargin(6)
    , _itemSpacing(6)
    , _enabled(true)
    , _mouseOver(false)
    , _active(false)
    , _top(false)
{
}

PrettyTab::~PrettyTab()
{
}

void PrettyTab::setEnabled(bool enabled)
{
    _enabled = enabled;
}

void PrettyTab::setSelected(bool selected)
{
    _active = selected;
    update();
}

void PrettyTab::setTop(bool top)
{
    _top = top;
}

void PrettyTab::clearSelection()
{
    setSelected(false);
}

void PrettyTab::enterEvent(QEvent *event)
{
    if(!_enabled)
    {
        event->ignore();
        return;
    }

    event->accept();
    _mouseOver = true;
    update();
}

void PrettyTab::leaveEvent(QEvent *event)
{
    if(!_enabled)
    {
        event->ignore();
        return;
    }

    event->accept();
    _mouseOver = false;
    update();
}

void PrettyTab::mousePressEvent(QMouseEvent *event)
{
    if(!_enabled)
    {
        event->ignore();
        return;
    }

    event->accept();
    _active = true;
    update();
    emit pressed();
}

void PrettyTab::paintEvent(QPaintEvent *event)
{
    event->accept();
    QPainter p(this);

    // Get the geometry of the object and then tweak to fit
    QRect size = geometry();
    size.moveTopLeft(QPoint(0, 0));
    size.setWidth(size.width()-2);
    size.setHeight(size.height()-1);

    // Check if we're active
    if(_active)
    {
        p.setBrush(QColor(255, 255, 255));
        p.setPen(QColor(255, 255, 255));

        p.drawRect(size);

        // For the active one, we should draw top and bottom lines
        p.setPen(QColor(204, 204, 204));

        if(!_top)
            p.drawLine(0, 0, size.width(), 0);
        p.drawLine(0, size.height(), size.width(), size.height());
    }
    // Still remember to check if it is not active, but hover-over
    else if(_mouseOver)
    {
        p.setBrush(QColor(204, 204, 204));
        p.setPen(QColor(204, 204, 204));

        p.drawRect(size);
    }

    p.setPen(QColor(10,10,10));

    QPixmap pixmap = _icon.pixmap(_iconWidth, _iconHeight);

    if(!_enabled)
    {
        p.setPen(QColor(100,100,100));
        // Make the pixmap grayscale
        QIcon icon = QIcon(pixmap);
        pixmap = icon.pixmap(QSize(_iconWidth, _iconHeight), QIcon::Disabled);
    }

    p.drawPixmap(_leftMargin, _topMargin, _iconWidth, _iconHeight, pixmap);
    p.drawText(
                QRectF(
                    _iconWidth + _leftMargin + _itemSpacing,
                    _topMargin,
                    p.fontMetrics().width(_label),
                    _iconHeight
                    ),
                _label,
                Qt::AlignLeft | Qt::AlignVCenter
                );
}

QSize PrettyTab::sizeHint() const
{
    // This cast should be safe
    QSize parentSize(0, 0);

    QFontMetrics fontMetrics = qApp->fontMetrics();
    int width = fontMetrics.width(_label) + _iconWidth + _leftMargin
            + _itemSpacing + _rightMargin;
    int height = qMax(_iconHeight, fontMetrics.height()) + _topMargin
            + _bottomMargin;

    return QSize(qMax(width, parentSize.width()), height);
}

}
