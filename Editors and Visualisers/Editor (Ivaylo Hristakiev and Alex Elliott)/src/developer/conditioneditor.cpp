/*!
 * \file
 */
#include "conditioneditor.hpp"
#include "conditionhighlighter.hpp"
#include "conditiontokens.hpp"

#include <QDebug>
#include <QRegExp>
#include <QSettings>
#include <QString>
#include <QToolTip>

namespace Developer {

ConditionEditor::ConditionEditor(QWidget *parent)
    : CodeEditor(parent)
    , _condition("")
    , _cache("")
    , _pos(0)
{
    _keywords << "edge" << "node" << "not" << "and" << "or" << "int" << "string"
              << "atom" << "empty" << "true" << "false" << "indeg" << "outdeg";

    // Mouse tracking is required for tooltips
    setMouseTracking(true);

    QSettings settings;
    QFont font = settings.value("Editor/Font", EDITOR_DEFAULT_FONT).value<QFont>();
    setFont(font);
    updateGutterWidth(blockCount());

    _highlighter = new ConditionHighlighter(this->document());
    _highlighter->setTokens(_tokens);
}

QRegExp ConditionEditor::pattern(int type) const
{
    switch(type)
    {
    case Empty:
        return QRegExp("empty");
    case Variable:
        return QRegExp("[a-z][a-zA-Z0-9_]{0,63}");
    case GraphLexeme:
        return QRegExp("[a-zA-Z0-9_]{1,63}");
    case ListSeparator:
        return QRegExp(":");
    case StringSeparator:
        return QRegExp("\\.");
    case Comma:
        return QRegExp(",");
    case OpeningParen:
        return QRegExp("\\(");
    case ClosingParen:
        return QRegExp("\\)");
    case DegreeTest:
        return QRegExp("indeg|outdeg");
    case Integer:
        return QRegExp("\\d+");
    case Plus:
        return QRegExp("\\+");
    case Minus:
    case Negation:
        return QRegExp("-");
    case Times:
        return QRegExp("\\*");
    case Divide:
        return QRegExp("/");
    case ConditionLexeme_CommentOpen:
        return QRegExp("/\\*");
    case ConditionLexeme_CommentClose:
        return QRegExp("\\*/");
    case LessThan:
        return QRegExp("<");
    case LessThanEqualTo:
        return QRegExp("<=");
    case GreaterThan:
        return QRegExp(">");
    case GreaterThanEqualTo:
        return QRegExp(">=");
    case Equals:
        return QRegExp("=");
    case NotEquals:
        return QRegExp("!=");
    case Not:
        return QRegExp("not");
    case And:
        return QRegExp("and");
    case Or:
        return QRegExp("or");
    case EdgeTest:
        return QRegExp("edge");
    case QuotedString:
        return QRegExp("\"[^\"]*\"");
    case ConditionLexeme_Comment:
    case ConditionLexeme_Default:
        return QRegExp();
    default:
        qDebug() << "Unknown type passed into ConditionEditor::pattern(): "
                    << type;
        return QRegExp();
    }
}

void ConditionEditor::parse()
{
    _condition = toPlainText();
    if(_cache == _condition)
        return;
    else
        _cache = _condition;

    _tokens.clear();
    _pos = 0;
    _openParens.clear();
    _wantsValue = true;

    // Do the actual parse
    while(_pos < _condition.length())
    {
        ConditionLexemes lexeme = ConditionLexeme_Error;
        int matchLength = 0;
        if(!findMatch(&lexeme, &matchLength))
        {
            lexeme = ConditionLexeme_Error;
            matchLength = 1;
        }

        handleLexeme(lexeme, matchLength);
    }

    for(int i = 0; i < _openParens.size(); ++i)
    {
        _openParens.at(i)->lexeme = ConditionLexeme_Error;
        _openParens.at(i)->description = tr("Unmatched opening parenthesis");
    }

    _highlighter->setTokens(_tokens);
    _highlighter->rehighlight();
}

bool ConditionEditor::findMatch(ConditionLexemes *lexeme,
                                int *matchLength, ConditionLexemes hint)
{
    if(_pos == _condition.length())
    {
        qDebug() << "findMatch() asked to find match beyond end of string";
        return false;
    }

    while(consumeWhitespace());

    QRegExp rx;
    *lexeme = ConditionLexeme_Error;
    *matchLength = 0;

    if(hint != ConditionLexeme_Default)
    {
        rx = pattern(hint);
        if(!rx.isEmpty())
        {
            if(rx.indexIn(_condition, _pos) == _pos)
            {
                *matchLength = rx.matchedLength();
                *lexeme = hint;
            }
        }
    }

    for(int i = 0; i < ConditionLexeme_Error; ++i)
    {
        // Get the regexp for this lexeme, ignore QRegExp() values
        rx = pattern(i);
        if(rx.isEmpty())
            continue;

        if(rx.indexIn(_condition, _pos) == _pos)
        {
            if(rx.matchedLength() > *matchLength)
            {
                *matchLength = rx.matchedLength();
                *lexeme = static_cast<ConditionLexemes>(i);
            }
        }
    }

    return (*matchLength > 0);
}

void ConditionEditor::handleLexeme(ConditionLexemes lexeme, int matchLength)
{
    QString text = _condition.mid(_pos, matchLength);
    Token *token = new Token();
    token->startPos = _pos;
    token->lexeme = lexeme;
    token->endPos = (_pos += matchLength);
    token->text = text;

    switch(lexeme)
    {
    case Not:
        // Accept it
        _tokens.push_back(token);
        return;
    case Empty:
        // Accept it
        _tokens.push_back(token);
        return;
    case And:
    case Or:
        // Accept it
        _tokens.push_back(token);
        return;
    case Variable:
        // Accept it
        _tokens.push_back(token);
        return;
    case GraphLexeme:
        // Reject it, we handle these in areas which accept them
        token->lexeme = ConditionLexeme_Error;
        token->description = tr("Unexpected identifier");
        _tokens.push_back(token);
        return;
    case StringSeparator:
            // Accept it
            _tokens.push_back(token);
            _wantsValue = true;
        return;
    case ListSeparator:
            // Accept it
            _tokens.push_back(token);
            _wantsValue = true;
        return;
    case Comma:
            // Accept it
            _tokens.push_back(token);
            return;
    case QuotedString:
        // Accept it
        _tokens.push_back(token);
        return;
    case OpeningParen:
        // Accept it
        _tokens.push_back(token);
        _openParens.push_back(token);
        return;
    case ClosingParen:
        if(_openParens.size() > 0)
        {
            _openParens.pop_back();
            _tokens.push_back(token);
        }
        else
        {
            token->lexeme = ConditionLexeme_Error;
            token->description = tr("Umatched closing parenthesis");
            _tokens.push_back(token);
        }
        return;
    case DegreeTest:
        // Accept it
        _tokens.push_back(token);
        return;
    case Integer:
        // Accept it
        _tokens.push_back(token);
        return;
//        // Reject it, we handle these in areas which accept them
//        token->lexeme = ConditionLexeme_Error;
//        token->description = tr("Unexpected integer");
//        _tokens.push_back(token);
//        return;
    case Negation:
        // Accept it
        _tokens.push_back(token);
        return;
    case Plus:
    case Minus:
    case Divide:
    case Times:
            // Accept it
            _tokens.push_back(token);
            return;
    case LessThan:
    case LessThanEqualTo:
    case GreaterThan:
    case GreaterThanEqualTo:
            // Accept it
            _tokens.push_back(token);
            return;
    case Equals:
    case NotEquals:
            // Accept it
            _tokens.push_back(token);
            return;
    case EdgeTest:
        // Accept it
        _tokens.push_back(token);
        parseEdgeTest();
        return;
    case ConditionLexeme_CommentOpen:
        delete token;
        _pos -= matchLength;
        consumeComments();
        return;
    case ConditionLexeme_CommentClose:
        // Reject it
        token->lexeme = ConditionLexeme_Error;
        token->description = tr("Unexpected comment close");
        _tokens.push_back(token);
        return;
    case ConditionLexeme_Error:
        // Accept it
        token->description = tr("No matches found at this position");
        _tokens.push_back(token);
        return;
    case ConditionLexeme_Default:
    case ConditionLexeme_Comment:
    default:
        // Shouldn't happen
        qDebug() << "Unhandled type entered: " << lexeme;
    }
}

void ConditionEditor::parseEdgeTest()
{
    ConditionLexemes lexeme;
    int matchLength = 0;
    int stage = 0;
    Token *token;
    while(_pos < _condition.length())
    {
        if(!findMatch(&lexeme, &matchLength, GraphLexeme))
        {
            if(stage > 0)
            {
                token = new Token();
                token->startPos = _pos;
                token->endPos = ++_pos;
                token->lexeme = ConditionLexeme_Error;
                token->description = tr("Unexpected character in edge test");
                _tokens.push_back(token);
                continue;
            }
            else
            {
                // Bail at stage 0, but not afterwards
                return;
            }
        }
        else
        {
            token = new Token();
            token->startPos = _pos;
            token->endPos = _pos += matchLength;
            token->lexeme = lexeme;
            token->text = _condition.mid(_pos, matchLength);
        }

        switch(stage)
        {
        case 0:
            if(lexeme == OpeningParen)
            {
                _tokens.push_back(token);
                ++stage;
            }
            else
            {
                // If we don't find an opening paren next, then we'll just move
                // on for now. Someone may be typing that at the moment and it
                // probably doesn't help to make everything afterwards an error
                delete token;
                _pos -= matchLength;
                return;
            }
            break;
        case 1:
        case 3:
            if(lexeme == GraphLexeme)
            {
                _tokens.push_back(token);
                ++stage;
                continue;
            }
            else
            {
                token->lexeme = ConditionLexeme_Error;
                _tokens.push_back(token);
                continue;
            }
            break;
        case 2:
            if(lexeme == Comma)
            {
                _tokens.push_back(token);
                ++stage;
                continue;
            }
            else
            {
                token->lexeme = ConditionLexeme_Error;
                _tokens.push_back(token);
                continue;
            }
            break;
        case 4:
            if(lexeme == ClosingParen)
            {
                _tokens.push_back(token);
                return;
            }
            else if(lexeme == Comma)
            {
                _tokens.push_back(token);
                ++stage;
                continue;
            }
            else
            {
//                qDebug() << " ConditionEditor: Stag " << stage << ", Lexeme" << lexeme;
                token->lexeme = ConditionLexeme_Error;
                //token->description = tr("Unexpected Special character in edge test");
                _tokens.push_back(token);
                continue;
            }
            break;
        case 5:
            // This should accepts a list
//            qDebug() << " ConditionEditor: Stage" << stage << ", Lexeme" << lexeme;

            if(lexeme == ClosingParen)
            {
                _tokens.push_back(token);
                return;
            }
            else
            {
                _pos += matchLength;
                //++stage;
            }

            break;
//        case 6:
//            if(lexeme == ClosingParen)
//            {
//                _tokens.push_back(token);
//                return;
//            }
//            else
//            {
//                qDebug() << " ConditionEditor: Stage" << stage << ", Lexeme" << lexeme;
//                token->lexeme = ConditionLexeme_Error;
//                token->description = tr("Unexpected Special character in edge test");
//                _tokens.push_back(token);
//                continue;
//            }
//            break;
        default:
            _pos -= matchLength;
            return;
        }
    }
}

bool ConditionEditor::consumeWhitespace()
{
    if(_pos >= _condition.length())
        return false;

    QRegExp rx("\\s+");
    if(rx.indexIn(_condition,_pos) == _pos)
    {
        Q_ASSERT(rx.matchedLength() > 0);
        _pos += rx.matchedLength();
        return true;
    }
    else
        return false;
}

bool ConditionEditor::consumeComments()
{
    if(_pos >= _condition.length())
        return false;

    // Check for a comment
    QRegExp rx = pattern(ConditionLexeme_CommentOpen);
    int matchPos = -1;
    Token *token = new Token;
    token->startPos = _pos;
    if((matchPos = rx.indexIn(_condition,_pos)) == _pos)
    {
        // Comment found, now we need to check for an ending and if we can't
        // find one then we just mark a comment to the end of the condition
        // and finish
        token->lexeme = ConditionLexeme_Comment;
        _pos += rx.matchedLength();
        rx = pattern(ConditionLexeme_CommentClose);
        if((matchPos = rx.indexIn(_condition, _pos)) > 0)
        {
            // We found a closing token, set the end position correctly and
            // advance the position
            token->text = _condition.mid(
                        _pos,
                        (matchPos-_pos) + rx.matchedLength());
            _pos = matchPos + rx.matchedLength();
            token->endPos = _pos;
            _tokens.push_back(token);
            return true;
        }
        else
        {
            // There was no closing token, match to the end of the string
            token->text = _condition.mid(
                        _pos,
                        _condition.length() - _pos);
            token->endPos = _condition.length();
            _pos = _condition.length();
            _tokens.push_back(token);
            return true;
        }
    }

    // No comment found
    delete token;
    return false;
}

void ConditionEditor::consumeError(const QString &expecting)
{
    if(_pos >= _condition.length())
        return;

    Token *error = new Token;
    error->lexeme = ConditionLexeme_Error;
    error->startPos = _pos;

    // Identifiers are the only contiguous segments
    QRegExp rx = pattern(Variable);
    if(rx.indexIn(_condition,_pos) != _pos)
    {
        // This isn't a block, move along one char
        error->text = QString(_condition.at(_pos));
        error->description = errorString(error->text, _pos) +
                expecting;
        error->endPos = ++_pos;
        _tokens.push_back(error);
        return;
    }

    error->text = rx.cap(0);
    error->endPos = _pos + rx.matchedLength();
    error->description = errorString(error->text, _pos) +
            expecting;
    _tokens.push_back(error);
    _pos += rx.matchedLength();
}

QString ConditionEditor::errorString(const QString &tokenFound, int position)
{
    QString ret =  tr("Unexpected token %1 at position %2. ").arg(
                tokenFound,
                QVariant(position).toString());
    //qDebug() << ret;
    return ret;
}

void ConditionEditor::mouseMoveEvent(QMouseEvent *e)
{
    QTextCursor textCursor = cursorForPosition(e->pos());
    textCursor.select(QTextCursor::WordUnderCursor);
    if(textCursor.selectedText().isEmpty())
        QToolTip::hideText();
    else
    {
        //! \todo Adapt this to handle error tokens /inside/ what the editor
        //! considers words
        // We have a word here, is it an error token?
        for(QVector<Token *>::iterator iter = _tokens.begin();
            iter != _tokens.end(); ++iter)
        {
            Token *t = *iter;
            if(textCursor.position() >= t->startPos
                    && textCursor.position() <= t->endPos)
            {
                if(t->lexeme == ConditionLexeme_Error)
                    QToolTip::showText(e->globalPos(), t->description);
            }
        }
    }
    CodeEditor::mouseMoveEvent(e);
}

}
