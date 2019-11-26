/*!
 * \file
 */
#include "appearancepreferences.hpp"
#include "ui_appearancepreferences.h"

#include <QFile>
#include <QDebug>

#include "programhighlighter.hpp"
#include "graph.hpp"

namespace Developer {

AppearancePreferences::AppearancePreferences(QWidget *parent)
    : PreferencesPage(parent)
    , _ui(new Ui::AppearancePreferences)
{
    _ui->setupUi(this);

    // Set up a simple program containing each of the token types
    QFile fp(":/templates/example_program.gpx");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString example = fp.readAll();

    _ui->plainTextEdit->setPlainText(example);
    _ui->plainTextEdit->parse();

    // Set up a simple graph
    Graph *lhsGraph = new Graph(":/templates/example_graph.host");
    _ui->lhsGraph->setGraph(lhsGraph);
    Graph *rhsGraph = new Graph(":/templates/example_graph_rhs.host");
    _ui->rhsGraph->setGraph(rhsGraph);

    reset();
}

AppearancePreferences::~AppearancePreferences()
{
    delete _ui;
}

void AppearancePreferences::reset()
{
    QSettings settings;

    // Empty this
    _ui->programTokenTable->clear();

    // Declare some variables which will be used to populate the table
    QTextCharFormat format;
    QTableWidgetItem *item;
    int row = 0;
    int column = 0;
    QStringList headers;

    headers << "Token" << "Font" << "Foreground" << "Background";
    _ui->programTokenTable->setHorizontalHeaderLabels(headers);

    QVector<int> tokens;
    // Default, identifiers, declaration identifiers, operators, keywords,
    // comments and errors
    tokens << ProgramLexeme_Default << ProgramLexeme_Identifier << ProgramLexeme_Declaration << ProgramLexeme_DeclarationOperator
           << ProgramLexeme_Keyword << ProgramLexeme_Comment << ProgramLexeme_Error;

    _ui->programTokenTable->setRowCount(tokens.count());

    for(row = 0; row < tokens.count(); ++row)
    {
        format = _ui->plainTextEdit->highlighter()->format(tokens.at(row));

        for(column = 0; column < 4; ++column)
        {
            item = new QTableWidgetItem();

            switch(column)
            {
            case 0:
                // The name of this token type
                switch(tokens.at(row))
                {
                case ProgramLexeme_Default:
                    item->setText("Default");
                    break;
                case ProgramLexeme_Identifier:
                    item->setText("Identifiers");
                    break;
                case ProgramLexeme_Declaration:
                    item->setText("Macro Declarations");
                    break;
                case ProgramLexeme_DeclarationOperator:
                    item->setText("Operators");
                    break;
                case ProgramLexeme_Keyword:
                    item->setText("Keywords");
                    break;
                case ProgramLexeme_Comment:
                    item->setText("Comments");
                    break;
                case ProgramLexeme_Error:
                    item->setText("Errors");
                    break;
                default:
                    qDebug() << "Unknown token type encountered in "
                                "AppearancePreferences::reset(), passed "
                             << tokens.at(row);
                    break;
                }

                item->setFlags(Qt::ItemIsSelectable);
                item->setFont(format.font());
                item->setForeground(format.foreground());
                item->setBackground(format.background());

                break;
            case 1:
                // The font
                item->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
                item->setText(format.font().family() + ", "
                              + QVariant(format.fontPointSize()).toString());
                break;
            case 2:
                // The foreground colour
                item->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEditable
                               | Qt::ItemIsEnabled);
                item->setText(QVariant(format.foreground().color()).toString());
                break;
            case 3:
                // The background colour
                item->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEditable
                               | Qt::ItemIsEnabled);
                item->setText(QVariant(format.background().color()).toString());
                break;
            default:
                qDebug() << "Unexpected column ID in "
                            "AppearancePreferences::reset(), passed " << column;
                break;
            }

            _ui->programTokenTable->setItem(row, column, item);
        }
    }

    // Now have the table rejig to fit the items it now contains
    _ui->programTokenTable->resizeColumnsToContents();
}

void AppearancePreferences::apply()
{
    QSettings settings;
}

}
