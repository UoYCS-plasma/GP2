/*!
 * \file
 */
#ifndef CODEEDITOR_HPP
#define CODEEDITOR_HPP

#include <QPlainTextEdit>
#include "token.hpp"
#include "global.hpp"

namespace Developer {

class CodeEditor;

/*!
 * \brief This class provides a "gutter" for line numbers and similar features
 *
 * The interface and approach is adapted from Qt's Code Editor example - the
 * write-up of which can be found here:
 * http://doc.qt.digia.com/4.7/widgets-codeeditor.html
 */
class CodeEditorGutter : public QWidget
{
    Q_OBJECT

public:
    explicit CodeEditorGutter(CodeEditor *editor = 0);

    QSize sizeHint() const;

protected:
    void paintEvent(QPaintEvent *event);

private:
    CodeEditor *_editor;
};

/*!
 * \brief The CodeEditor class provides an abstract base class for code editors
 *
 * In GP this is used for the ProgramEditor and the ConditionEditor.
 */
class CodeEditor : public QPlainTextEdit
{
    Q_OBJECT

public:
    explicit CodeEditor(QWidget *parent);

    /*!
     * \brief Gets the regular expression associated with the provided type
     * \param type  The type of the token whose regexp we are retrieving. This
     *  will be a value from the Lexemes enum
     * \return A QRegExp object containing a pattern which will match this type
     *  of lexeme
     */
    virtual QRegExp pattern(int type) const = 0;

    int gutterWidth() const;
    void drawGutter(QPaintEvent *event);

public slots:
    virtual void parse() = 0;

protected slots:
    void updateGutterWidth(int blockCount);
    void updateGutter(const QRect &rect, int scrollDistance);

protected:
    void resizeEvent(QResizeEvent *event);

    QVector<Token *> _tokens;
    QStringList _keywords;
    CodeEditorGutter *_gutter;
};

}

#endif // CODEEDITOR_HPP
