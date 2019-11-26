/*!
 * \file
 */
#include "programedit.hpp"
#include "ui_programedit.h"

#include "program.hpp"

#include <QDebug>
#include <QSettings>
#include <QFont>

namespace Developer {

ProgramEdit::ProgramEdit(QWidget *parent)
    : QWidget(parent)
    , _ui(new Ui::ProgramEdit)
    , _program(0)
    , _programCache("")
    , _documentationCache("")
    , _setUp(false)
{
    _ui->setupUi(this);
}

ProgramEdit::~ProgramEdit()
{
    delete _ui;
}

void ProgramEdit::setProgram(Program *program)
{
    if(program == 0)
    {
        qDebug() << "ProgramEdit::setProgram() received a null pointer. Ignoring.";
        return;
    }

    _setUp = false;
    _program = program;
    _programCache = _program->program();
    _documentationCache = _program->documentation();
    //_ui->documentationEdit->setPlainText(_program->documentation());
    _ui->editor->setPlainText(_program->program());
    _ui->editor->parse();
    _setUp = true;

    //_ui->documentationEdit->setEnabled(_program->status() != GPFile::ReadOnly);
    _ui->editor->setEnabled(_program->status() != GPFile::ReadOnly);
}

void ProgramEdit::textEdited()
{
    if(!_setUp)
        return;

    QString prog = _ui->editor->toPlainText();
    //QString docs = _ui->documentationEdit->toPlainText();
    if(_program != 0
            && _programCache != prog)
    {
        _ui->editor->parse();
        _programCache = prog;
        _program->setProgram(prog);
    }
    /*else if(_program != 0
            && _documentationCache != docs)
    {
        _programCache = docs;
        _program->setDocumentation(docs);
    }*/
}

}
