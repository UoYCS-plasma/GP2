/*!
 * \file
 */
#include "codeeditor.hpp"

#include <QDebug>
#include <QTextBlock>
#include <QResizeEvent>
#include <QPaintEvent>
#include <QPainter>
#include <cmath>

namespace Developer {

CodeEditorGutter::CodeEditorGutter(CodeEditor *editor)
    : QWidget(editor)
    , _editor(editor)
{
}

QSize CodeEditorGutter::sizeHint() const
{
    return QSize(_editor->gutterWidth(),0);
}

void CodeEditorGutter::paintEvent(QPaintEvent *event)
{
    _editor->drawGutter(event);
}

CodeEditor::CodeEditor(QWidget *parent)
    : QPlainTextEdit(parent)
{
    _gutter = new CodeEditorGutter(this);

    connect(this, SIGNAL(blockCountChanged(int)),
            this, SLOT(updateGutterWidth(int)));
    connect(this, SIGNAL(updateRequest(QRect,int)),
            this, SLOT(updateGutter(QRect,int)));

    updateGutterWidth(0);
}

int CodeEditor::gutterWidth() const
{
    int characters = 1;
    if(blockCount() > 0)
    {
        double w = std::log10(static_cast<double>(blockCount()));
        characters = static_cast<int>(std::floor(w)) + 1;
    }

    // Add another char width as padding
    ++characters;

    int gWidth = 4 + fontMetrics().width(QString(characters, QChar('9')));

    Q_ASSERT(gWidth > 0);

    return gWidth;
}

void CodeEditor::updateGutterWidth(int blockCount)
{
    Q_UNUSED(blockCount)
    setViewportMargins(gutterWidth(), 0, 0, 0);
}

void CodeEditor::updateGutter(const QRect &rect, int scrollDistance)
{
    if(scrollDistance)
        _gutter->scroll(0, scrollDistance);
    else
        _gutter->update(0, rect.y(), _gutter->width(), rect.height());

    if(rect.contains(viewport()->rect()))
        updateGutterWidth(0);
}

void CodeEditor::resizeEvent(QResizeEvent *event)
{
    QPlainTextEdit::resizeEvent(event);

    QRect rect = contentsRect();
    _gutter->setGeometry(
                QRect(rect.left(), rect.top(), gutterWidth(), rect.height())
                );
}

void CodeEditor::drawGutter(QPaintEvent *event)
{
    QPainter painter(_gutter);
    if(!painter.isActive())
        return;

    painter.setFont(font());
    painter.fillRect(event->rect(), Qt::lightGray);
    painter.setPen(QColor(0x4f,0x4f,0x4f));

    if(blockCount() < 1)
        return;

    QTextBlock block = firstVisibleBlock();
    int number = block.blockNumber();
    int top = static_cast<int>(
                blockBoundingGeometry(block).translated(contentOffset()).top()
                );
    int bottom = top + static_cast<int>(blockBoundingRect(block).height());

    while (block.isValid() && top <= event->rect().bottom())
    {
        if (block.isVisible() && bottom >= event->rect().top())
        {
            QString numberString = QVariant(number + 1).toString();
            painter.drawText(0, top, _gutter->width()-4,
                             fontMetrics().height(), Qt::AlignRight, numberString);
        }

        block = block.next();
        top = bottom;
        bottom = top + static_cast<int>(blockBoundingRect(block).height());
        ++number;
    }
}

}
