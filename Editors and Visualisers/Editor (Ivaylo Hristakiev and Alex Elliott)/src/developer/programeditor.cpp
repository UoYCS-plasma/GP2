/*!
 * \file
 */
#include "programeditor.hpp"
#include "programhighlighter.hpp"

#include <QSettings>
#include <QDebug>
#include <QEvent>
#include <QToolTip>

namespace Developer {

ProgramEditor::ProgramEditor(QWidget *parent)
    : CodeEditor(parent)
    , _program("")
    , _pos(0)
    , _scopeDepth(0)
    , _cache("")
{
    // Set the list of keywords
    _keywords << "Main" << "if" << "try" << "then" << "else" << "skip" << "fail"
              << "or" << "break";

    // Mouse tracking is required for tooltips
    setMouseTracking(true);

    QSettings settings;
    QFont font = settings.value("Editor/Font", EDITOR_DEFAULT_FONT).value<QFont>();
    setFont(font);
    _highlighter = new ProgramHighlighter(document());
    _highlighter->setTokens(_tokens);
}

void ProgramEditor::parse()
{
    _program = toPlainText();
    if(_program.isEmpty())
        return;

    // Don't run this again if the program hasn't changed
    if(_cache == _program)
        return;
    else
        _cache = _program;

    // Clear the token vector, we're going to repopulate it in this pass
    for(int i = 0; i < _tokens.count(); ++i)
        delete _tokens.at(i);
    _tokens.clear();

    _pos = 0;
    parseDeclarations();

    if(_pos != _program.length())
    {
        qDebug() << "Parsing finished before the end of the input. Stopped at:"
                 << _pos;
    }

    _highlighter->setTokens(_tokens);
    _highlighter->rehighlight();
}

ProgramHighlighter *ProgramEditor::highlighter() const
{
    return _highlighter;
}

bool ProgramEditor::consumeWhitespace()
{
    QRegExp rx("\\s+");
    if(rx.indexIn(_program,_pos) == _pos)
    {
        Q_ASSERT(rx.matchedLength() > 0);
        _pos += rx.matchedLength();
        return true;
    }
    else
        return false;
}

bool ProgramEditor::consumeComments()
{
    // Check for a comment
    QRegExp rx = pattern(ProgramLexeme_CommentOpen);
    int matchPos = -1;
    Token *token = new Token;
    token->startPos = _pos;
    if((matchPos = rx.indexIn(_program,_pos)) == _pos)
    {
        // Comment found, now we need to check for an ending and if we can't
        // find one then we just mark a comment to the end of the program
        // and finish
        token->lexeme = ProgramLexeme_Comment;
        _pos += rx.matchedLength();
        rx = pattern(ProgramLexeme_CommentClose);
        if((matchPos = rx.indexIn(_program, _pos)) > 0)
        {
            // We found a closing token, set the end position correctly and
            // advance the position
            token->text = _program.mid(
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
            token->text = _program.mid(
                        _pos,
                        _program.length() - _pos);
            token->endPos = _program.length();
            _pos = _program.length();
            _tokens.push_back(token);
            return true;
        }
    }

    // No comment found
    delete token;
    return false;
}

void ProgramEditor::consumeError(const QString &expecting)
{
    Token *error = new Token;
    error->lexeme = ProgramLexeme_Error;
    error->startPos = _pos;

    // Identifiers are the only contiguous segments
    QRegExp rx = pattern(ProgramLexeme_Identifier);
    if(rx.indexIn(_program,_pos) != _pos)
    {
        // This isn't a block, move along one char
        error->text = QString(_program.at(_pos));
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

void ProgramEditor::parseDeclarations()
{
    // We want to loop until we reach the end of this scope or we overshoot the
    // end of the string
    bool finished = false;
    bool canExit = true;
    QRegExp rx;
    int previousPosition = -1;
    while(!finished && _pos < _program.length())
    {
        if(previousPosition == _pos)
        {
            qDebug() << "Parsing ended before end of string. Ended at " << _pos;
            return;
        }
        else
            previousPosition = _pos;

        // Ignore whitespace and comments
        if(consumeComments() || consumeWhitespace())
            continue;

        // Check for a closing paren, if we are within a scope then this brings
        // us up one.
        if(_pos < _program.length() && _program.at(_pos) == QChar(')'))
        {
            Token *token = new Token;
            token->startPos = _pos;
            token->endPos = ++_pos;
            token->text = ")";

            // If we can exit then record that token and do so
            if(canExit)
            {
                // Check if there is a parent scope to exit to
                if(_scopeDepth > 0)
                {
                    token->lexeme = ProgramLexeme_CloseParen;
                    _tokens.push_back(token);
                    return;
                }
                else
                {
                    token->lexeme = ProgramLexeme_Error;
                    token->description = errorString("CloseParen", _pos) + tr(
                                "No parent scope to exit to");
                }
            }
            else
            {
                token->lexeme = ProgramLexeme_Error;
                token->description = errorString("CloseParen",_pos) + tr(
                            "Scope cannot be exited here.");
            }
            _tokens.push_back(token);
            continue;
        }

        // This is now non-whitespace and not a comment, it should be an
        // identifier or main:
        Token *declarationName = new Token;
        declarationName->startPos = _pos;

        // Check for "main"
        rx = QRegExp("Main\\b");
        QRegExp keywords = pattern(ProgramLexeme_Keyword);
        if(rx.indexIn(_program,_pos) == _pos)
        {
            declarationName->lexeme = ProgramLexeme_Keyword;
        }
        // Not main. Check for a declaration
        else if((rx = pattern(ProgramLexeme_Declaration)).indexIn(_program,_pos) == _pos
                && !keywords.exactMatch(rx.cap(0)))
        {
            declarationName->lexeme = ProgramLexeme_Declaration;
            canExit = false;
        }
        // Not either, it's an error
        else
        {
            delete declarationName;
            consumeError(tr("Expected Declaration or 'Main'"));
            continue;
        }

        // We now have a declaration or main
        declarationName->text = rx.cap(0);
        declarationName->endPos = _pos + rx.matchedLength();
        _tokens.push_back(declarationName);
        _pos += rx.matchedLength();
        canExit = false;

        // An assignment operator is now required
        while(_pos < _program.length() && _program.at(_pos) != QChar('='))
        {
            if(consumeWhitespace() || consumeComments())
                continue;
            consumeError(tr("Expected '=' operator."));
        }

        // Are we at the end of the string? If not we've found it
        if(_pos != _program.length())
        {
            // Now we have found our equals check if the previous token was an
            // identifier, if yes then mark it as a declaration
            if(_tokens.back()->lexeme == ProgramLexeme_Identifier)
                _tokens.back()->lexeme = ProgramLexeme_Declaration;

            Token *token = new Token;
            token->startPos = _pos;
            token->endPos = ++_pos;
            token->lexeme = ProgramLexeme_DeclarationOperator;
            token->text = "=";
            _tokens.push_back(token);

            // Ok, now that we've hit a declaration we can actually proceed
            parseCommandSeqence();

            // Once we return from the command sequence a full stop is required
            /*while(_pos < _program.length() && _program.at(_pos) != QChar('.'))
            {
                if(consumeWhitespace() || consumeComments())
                    continue;
                consumeError(tr("Expected '.' terminator."));
            }*/
						while (consumeWhitespace() || consumeComments()) ;

            // Similar to the above, if this is not the EOF then we've found it
            /*if(_pos != _program.length())
            {
                token = new Token;
                token->startPos = _pos;
                token->endPos = ++_pos;
                token->lexeme = ProgramLexeme_DeclarationSeparator;
                token->text = ".";
                _tokens.push_back(token);
            }*/
        }
    }
}

void ProgramEditor::parseCommandSeqence()
{
    // Termination of a command sequence occurs when any of these are found:
    //  - the declaration separator: .
    //  - a block end: )
    //  - end of string
    bool wantCommand = true;
    while(_pos != _program.length() //&& _program.at(_pos) != QChar('.')
          && _program.at(_pos) != QChar(')'))
    {
        if(consumeWhitespace() || consumeComments())
            continue;

        // Read in a command
        if(wantCommand)
        {
            parseCommand();
            wantCommand = false;
        }
        // We now need to terminate or reach a statement separator
        else
        {
            if(_program.at(_pos) == QChar(';'))
            {
                Token *token = new Token;
                token->startPos = _pos;
                token->endPos = ++_pos;
                token->lexeme = ProgramLexeme_StatementSeparator;
                token->text = ";";
                _tokens.push_back(token);
                wantCommand = true;
            }
            else
								// We dont have a command next and a separator ';' is not found - the sequence has ended
                //consumeError("Expecting statement separator (;)");
								return;
        }
    }

    // If wantCommand is true check if the last token was a separator - if it
    // was then this is an error by our grammar
    if(wantCommand && _tokens.back()->text == ";")
    {
        _tokens.back()->lexeme = ProgramLexeme_Error;
        _tokens.back()->description = errorString("StatementSeparator", _pos-1)
                + tr("No statement following this separator");
    }
}

void ProgramEditor::parseCommand()
{
    // A command may be:
    // - Block { '!' }
    // - Block 'or' Block
    // - if Block then Block { else Block }
    // - try Block { then Block { else Block } }

    // Make sure we're at the first token in the command
    while(consumeWhitespace() || consumeComments());

    // Check for if or try first
    QRegExp ifExp("if\\b");
    QRegExp tryExp("try\\b");
    if(ifExp.indexIn(_program,_pos) == _pos)
        parseIf();
    else if(tryExp.indexIn(_program, _pos) == _pos)
        parseTry();
    else
    {
        // Take the first block
        parseBlock();

        // We now might either have a '!' or 'or', but first (as always):
        while(consumeWhitespace() || consumeComments());

        QRegExp orExp = QRegExp("or\\b");
        if(_pos != _program.length() && _program.at(_pos) == QChar('!'))
        {
            Token *token = new Token;
            token->startPos = _pos;
            token->endPos = ++_pos;
            token->lexeme = ProgramLexeme_Repeat;
            token->text = "!";
            _tokens.push_back(token);
        }
        else if(orExp.indexIn(_program, _pos) == _pos)
        {
            Token *token = new Token;
            token->startPos = _pos;
            token->endPos = _pos + orExp.matchedLength();
            token->lexeme = ProgramLexeme_Keyword;
            token->text = "or";
            _tokens.push_back(token);
            _pos += orExp.matchedLength();

            // This is followed by another block
            parseBlock();
        }
    }
}

void ProgramEditor::parseIf()
{
    // To get here it should be the case that the first token in the program is
    // the initial 'if' keyword.
    QRegExp rx("if\\b");
    if(rx.indexIn(_program,_pos) != _pos)
    {
        qDebug() << "Program parser error in parseIf(): no if keyword found at "
                    "position " << _pos;
        return;
    }

    Token *token = new Token;
    token->startPos = _pos;
    token->endPos = (_pos += rx.matchedLength());
    token->lexeme = ProgramLexeme_Keyword;
    token->text = "if";
    _tokens.push_back(token);

    // Then a block follows
    parseBlock();

    // Now we expect a 'then' keyword. Look for it
    rx = QRegExp("then\\b");
    while(_pos < _program.length() && rx.indexIn(_program,_pos) != _pos)
    {
        if(consumeWhitespace() || consumeComments())
            continue;
        else
            consumeError("Expecting 'then' keyword.");
    }

    token = new Token;
    token->startPos = _pos;
    token->endPos = (_pos += rx.matchedLength());
    token->lexeme = ProgramLexeme_Keyword;
    token->text = "then";
    _tokens.push_back(token);

    // Another block follows
    parseBlock();

    // The 'else' portion is optional, check for it but just exit if it is not
    // present
    while(consumeWhitespace() || consumeComments());

    rx = QRegExp("else\\b");
    if(rx.indexIn(_program,_pos) == _pos)
    {
        token = new Token;
        token->startPos = _pos;
        token->endPos = (_pos += rx.matchedLength());
        token->lexeme = ProgramLexeme_Keyword;
        token->text = "else";
        _tokens.push_back(token);

        // And a final block
        parseBlock();
    }
}

void ProgramEditor::parseTry()
{
    // To get here it should be the case that the first token in the program is
    // the initial 'if' keyword.
    QRegExp rx("try\\b");
    if(rx.indexIn(_program,_pos) != _pos)
    {
        qDebug() << "Program parser error in parseTry(): no try keyword found "
                    "at position " << _pos;
        return;
    }

    Token *token = new Token;
    token->startPos = _pos;
    token->endPos = (_pos += rx.matchedLength());
    token->lexeme = ProgramLexeme_Keyword;
    token->text = "try";
    _tokens.push_back(token);

    // Then a block follows
    parseBlock();

    // The 'then' portion is optional, check for it but just exit if it is not
    // present
    while(consumeWhitespace() || consumeComments());

    QRegExp rxThen = QRegExp("then\\b");
    QRegExp rxElse = QRegExp("else\\b");
    if(rxThen.indexIn(_program,_pos) == _pos)
    {
        token = new Token;
        token->startPos = _pos;
        token->endPos = (_pos += rxThen.matchedLength());
        token->lexeme = ProgramLexeme_Keyword;
        token->text = "then";
        _tokens.push_back(token);

        // Another block follows
        parseBlock();

        // The 'else' portion is optional, check for it but just exit if it is
        // not present
        while(consumeWhitespace() || consumeComments());

        if(rxElse.indexIn(_program,_pos) == _pos)
        {
            token = new Token;
            token->startPos = _pos;
            token->endPos = (_pos += rxElse.matchedLength());
            token->lexeme = ProgramLexeme_Keyword;
            token->text = "else";
            _tokens.push_back(token);

            // And a final block
            parseBlock();
        }
    }
		else if(rxElse.indexIn(_program,_pos) == _pos)
    {
        token = new Token;
        token->startPos = _pos;
        token->endPos = (_pos += rxElse.matchedLength());
        token->lexeme = ProgramLexeme_Keyword;
        token->text = "else";
        _tokens.push_back(token);

        // And a final block
        parseBlock();
    }
}

void ProgramEditor::parseBlock()
{
    // A block can be:
    //  - '(' CommandSequence ')' { '!' }
    //  - RuleSetCall (this covers calling single rules as well)
    //  - MacroCall (syntactically exactly the same as a single rule call)
    //  - skip
    //  - fail
    //  - break

    // Make sure we're starting at the first real token
    while(_pos < _program.length())
    {
        if(consumeWhitespace() || consumeComments())
            continue;

        Token *token = new Token;
        token->startPos = _pos;

        // Check for keywords
        QRegExp rx("(skip|fail|break)\\b");
        if(rx.indexIn(_program,_pos) == _pos)
        {
            token->endPos = (_pos += rx.matchedLength());
            token->text = rx.cap(0);
            token->lexeme = ProgramLexeme_Keyword;
            _tokens.push_back(token);
            return;
        }

        // Check for an identifier
        rx = pattern(ProgramLexeme_Identifier);
        QRegExp keywords = pattern(ProgramLexeme_Keyword);
        if(rx.indexIn(_program,_pos) == _pos)
        {
            token->endPos = (_pos += rx.matchedLength());
            token->text = rx.cap(0);
            if(keywords.exactMatch(token->text))
            {
                token->lexeme = ProgramLexeme_Error;
                token->description = errorString("Keyword", token->startPos)
                        + tr("Identifiers cannot be keywords");
            }
            else
                token->lexeme = ProgramLexeme_Identifier;
            _tokens.push_back(token);
            return;
        }

        // Check for a declaration
        rx = pattern(ProgramLexeme_Declaration);
        if(rx.indexIn(_program,_pos) == _pos)
        {
            token->endPos = (_pos += rx.matchedLength());
            token->text = rx.cap(0);
            if(keywords.exactMatch(token->text))
            {
                token->lexeme = ProgramLexeme_Error;
                token->description = errorString("Keyword", token->startPos)
                        + tr("Identifiers cannot be keywords");
            }
            else
                token->lexeme = ProgramLexeme_Declaration;
            _tokens.push_back(token);
            return;
        }

        // Check for an open paren
        if(_pos < _program.length() && _program.at(_pos) == QChar('('))
        {
            token->endPos = ++_pos;
            token->text = "(";
            token->lexeme = ProgramLexeme_OpenParen;
            _tokens.push_back(token);

            // We now expect a command sequence
            parseCommandSeqence();

            // Once we're back we require a closing parenthesis - if we can't
            // find one just mark the opening parenthesis as unmatched - it's
            // simpler than marking the entire rest of the program.
            while(consumeWhitespace() || consumeComments());
            if(_pos < _program.length() && _program.at(_pos) == QChar(')'))
            {
                token = new Token;
                token->startPos = _pos;
                token->endPos = ++_pos;
                token->text = ")";
                token->lexeme = ProgramLexeme_CloseParen;
                _tokens.push_back(token);

            		while(consumeWhitespace() || consumeComments());
								if(_pos != _program.length() && _program.at(_pos) == QChar('!'))
								{
								    Token *token = new Token;
								    token->startPos = _pos;
								    token->endPos = ++_pos;
								    token->lexeme = ProgramLexeme_Repeat;
								    token->text = "!";
								    _tokens.push_back(token);
								}
            }
            else
            {
                token->lexeme = ProgramLexeme_Error;
                token->description = errorString("OpenParen", token->startPos)
                        + tr("Unmatched parenthesis");
            }
            return;
        }

        // Check for an opening curly brace
        if(_pos < _program.length() && _program.at(_pos) == QChar('{'))
        {
            parseRuleSet();
            return;
        }

        // No match, consume an error and continue looking
        delete token;
        consumeError("Expecting command.");
    }
}

void ProgramEditor::parseRuleSet()
{
    // Move to the first real token if we're not already there
    while(consumeWhitespace() || consumeComments());

    if(_pos < _program.length() && _program.at(_pos) == QChar('{'))
    {
        Token *token = new Token;
        token->startPos = _pos;
        token->endPos = ++_pos;
        token->lexeme = ProgramLexeme_OpenBrace;
        token->text = "{";
        _tokens.push_back(token);

        bool wantsRule = true;
        while(_pos < _program.length())
        {
            if(consumeWhitespace() || consumeComments())
                continue;

            token = new Token;
            token->startPos = _pos;

            // Finish upon finding a closing curly brace
            if(_pos < _program.length() && _program.at(_pos) == QChar('}'))
            {
                // Handle a trailing comma for the user
                if(wantsRule && _tokens.back()->text == ",")
                {
                    _tokens.back()->lexeme = ProgramLexeme_Error;
                    _tokens.back()->description = errorString(
                                "RuleSeparator",
                                _tokens.back()->startPos) + tr("Separator is not "
                                                               "followed by a rule.");
                }

                token->endPos = ++_pos;
                token->text = "}";
                token->lexeme = ProgramLexeme_CloseBrace;
                _tokens.push_back(token);
                break;
            }

            // We haven't ended, take a rule identifier or a comma as required
            if(wantsRule)
            {
                QRegExp rx = pattern(ProgramLexeme_Identifier);
                QRegExp keywords = pattern(ProgramLexeme_Keyword);
                if(rx.indexIn(_program,_pos) == _pos)
                {
                    token->endPos = (_pos += rx.matchedLength());
                    token->text = rx.cap(0);
                    if(keywords.exactMatch(token->text))
                    {
                        token->lexeme = ProgramLexeme_Error;
                        token->description = errorString("Keyword", token->startPos)
                                + tr("Identifiers cannot be keywords");
                    }
                    else
                        token->lexeme = ProgramLexeme_Identifier;
                    _tokens.push_back(token);
                    wantsRule = false;
                    continue;
                }
            }
            else
            {
                if(_pos < _program.length() && _program.at(_pos) == QChar(','))
                {
                    token->endPos = ++_pos;
                    token->text = ",";
                    token->lexeme = ProgramLexeme_RuleSeparator;
                    _tokens.push_back(token);
                    wantsRule = true;
                    continue;
                }
            }

            delete token;
            consumeError("Expecting comma-delimited list of rules.");
        }
    }
    else
    {
        qDebug() << "Program parser error: parseRuleSet() called but unable to "
                    "find a '{' character at position: " << _pos;
        return;
    }
}

QString ProgramEditor::errorString(const QString &tokenFound, int position)
{
    QString ret =  tr("Unexpected token %1 at position %2. ").arg(
                tokenFound,
                QVariant(position).toString());
    //qDebug() << ret;
    return ret;
}

QRegExp ProgramEditor::pattern(int type) const
{
    switch(type)
    {
    //case ProgramLexeme_Comment:
        //return QRegExp("\\\\([^\n])+");
    case ProgramLexeme_CommentOpen:
        return QRegExp("\\/\\/([^\n])*");
    case ProgramLexeme_CommentClose:
        return QRegExp("\n"); 
    case ProgramLexeme_Keyword:
        return QRegExp(_keywords.join("\\b|"));
    case ProgramLexeme_Declaration:
        return QRegExp("[A-Z][a-zA-Z0-9_]{,62}");
    case ProgramLexeme_Identifier:
        return QRegExp("[a-z][a-zA-Z0-9_]{,62}");
    case ProgramLexeme_DeclarationOperator:
        return QRegExp("=");
    case ProgramLexeme_OpenParen:
        return QRegExp("\\(");
    case ProgramLexeme_CloseParen:
        return QRegExp("\\)");
    case ProgramLexeme_OpenBrace:
        return QRegExp("\\{");
    case ProgramLexeme_CloseBrace:
        return QRegExp("\\}");
    case ProgramLexeme_Repeat:
        return QRegExp("!");
    case ProgramLexeme_StatementSeparator:
        return QRegExp(";");
    case ProgramLexeme_Default:
    case ProgramLexeme_Error:
    default:
        qDebug() << "Attempted to find pattern for invalid type: " << type;
        return QRegExp();
    }
}

void ProgramEditor::mouseMoveEvent(QMouseEvent *e)
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
                if(t->lexeme == ProgramLexeme_Error)
                    QToolTip::showText(e->globalPos(), t->description);
            }
        }
    }
    CodeEditor::mouseMoveEvent(e);
}

}
