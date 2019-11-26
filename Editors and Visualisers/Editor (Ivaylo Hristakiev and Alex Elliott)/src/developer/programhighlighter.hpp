/*!
 * \file
 */
#ifndef PROGRAMHIGHLIGHTER_HPP
#define PROGRAMHIGHLIGHTER_HPP

#include "programtokens.hpp"
#include "token.hpp"

#include <QSyntaxHighlighter>
#include <QRegExp>

namespace Developer {

/*!
 * \brief The ProgramHighlighter class provides a QSyntaxHighlighter for GP
 *  Programs
 *
 * The normal method suggested by the QSyntaxHighlighter examples and class
 * interface work on a "text block" basis along with an integer block state
 * attached to each block. This allows for text to be re-highlighted on a
 * per-block basis to reduce overhead in large documents. The state passes along
 * information such as "in a multi-line comment block" to subsequent states and
 * this is sufficient for many simple use-cases.
 *
 * The approach adopted by Qt for syntax highlighting is very good for a simple
 * forward-parse, if I only wished to produce a token sequence without checking
 * for the validity of the tokens in that sequence then the method described
 * would be entirely valid. However, since I do want to mark obviously incorrect
 * code across the whole document it is necessary to consider the entirity of
 * the code when determining whether certain tokens are valid or should be
 * marked as errors.
 *
 * An example of this is brace and parenthesis matching. When expressions can
 * span many code blocks it is not possible to scan ahead through the whole
 * document to determine if there is a matching close, similarly this approach
 * makes it very difficult to know how many opening braces/parens have occured
 * in previous blocks.
 *
 * After it was decided that the document should be subject to one single parse
 * rather than the block-based model that Qt suggests the question then becomes
 * how this should be achieved. The Qt interface is as it is because of
 * performance concerns in large documents. It is possible to mitigate this by
 * a very sophisticated model which is able to isolate changes based on the
 * lexical structure of the program - but actually the performance issue is
 * (for the most part) a non-issue in the case of implementing a GP program
 * editor. The program would have to be excessively large for this parsing step
 * to cause editing delay, and if it does become an issue in the future then I
 * would suggest that the re-highlighting would be put on a delay in a
 * background thread as an easier solution than a massively sophisticated parser
 * with update support.
 *
 * Therefore this class takes the simple but effective approach of parsing the
 * document when things are changed, that occurs in parseProgram() which produces
 * a vector of ProgramHighlighter::Token instances. The highlightBlock() portion
 * is now an interface onto that token vector, each Token is checked to see if
 * it applies to the block in question, and if yes the correct portion is
 * coloured according to the format() member function.
 *
 * \todo Alter the above to reflect that parsing occurs in ProgramEditor now
 */
class ProgramHighlighter : public QSyntaxHighlighter
{
    Q_OBJECT

public:
    /*!
     * \brief Construct a new ProgramHighlighter for a given text document
     * \param programText   The initial program text
     * \param parent        The text document this highlighter works upon
     */
    explicit ProgramHighlighter(QTextDocument *parent = 0);

    /*!
     * \brief Gets the format associated with the provided type
     * \param type  The type of the token whose format we are retrieving. This
     *  will be a value from the Lexemes enum
     * \return The QTextCharFormat associated with this lexeme
     */
    QTextCharFormat format(int type) const;

public slots:
    /*!
     * \brief Set the sequence of tokens which correspond with this program
     * \param tokens    The sequence of tokens
     */
    void setTokens(QVector<Token *> tokens);
    /*!
     * \brief A wrapper function which complies with the standard
     *  QSyntaxHighlighter interface
     * \param text  The text contents of the current block being highlighted
     */
    void highlightBlock(const QString &text);

private:
    QVector<Token *> _tokens;
};

}

#endif // PROGRAMHIGHLIGHTER_HPP
