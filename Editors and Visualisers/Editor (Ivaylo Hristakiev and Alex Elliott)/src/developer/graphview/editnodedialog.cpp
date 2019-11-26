/*!
 * \file
 */
#include "editnodedialog.hpp"
#include "ui_editnodedialog.h"

#include "nodeitem.hpp"
#include "node.hpp"
#include "graph.hpp"
#include "listvalidator.hpp"

#include <QFile>
#include <QDebug>
#include <QSettings>
#include <QRegExp>

namespace Developer {

EditNodeDialog::EditNodeDialog(NodeItem *node, QWidget *parent)
    : QDialog(parent)
    , _ui(new Ui::EditNodeDialog)
    , _node(node)
{
    _ui->setupUi(this);

    // Load the help stylesheet and apply it to this widget
    QFile fp(":/stylesheets/main.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);


    bool graphIsRule = _node->node()->parent()->isRuleGraph();

    if (graphIsRule)
        _idValidator = new QRegExpValidator(QRegExp("[a-z][a-zA-Z0-9_]*|[0-9]+"), this);
    else
        _idValidator = new QRegExpValidator(QRegExp("[0-9]+"), this);

    _ui->idEdit->setText(node->id());
    _ui->idEdit->setValidator(_idValidator);
    _ui->rootCheckBox->setChecked(node->isRoot());

    if (!graphIsRule)
    {
        _ui->idEdit->hide();
        _ui->idLabel->hide();
        _ui->rootCheckBox->hide();
        _ui->rootLabel->hide();
    }


    _ui->labelEdit->setText(node->label());

    _ui->markComboBox->addItem("none");//set our icon

    QSettings settings;
    //create pixmap and choose size
    QPixmap px(15,15);
    QColor color;

    color = settings.value("GraphView/Nodes/ColourAny",
                           QColor(0xee,0x82,0xee) // purple-ish
                           ).value<QColor>();
    px.fill(color);
    QIcon icon(px);

    // Only rule graphs have 'any' mark
    if (graphIsRule)
        _ui->markComboBox->addItem(icon,"any");

    color = settings.value("GraphView/Nodes/ColourShaded",
                           QColor(0xb2,0xb2,0xb2) // light gray
                           ).value<QColor>();
    px.fill(color);  //use our pixmap again, but with another color
    icon.addPixmap(px);         //use our icon again
    _ui->markComboBox->addItem(icon,"grey");

    color = settings.value("GraphView/Nodes/ColourRed",
                           QColor(0xee,0x77,0x77) // light red
                           ).value<QColor>();
    px.fill(color);  //use our pixmap again, but with another color
    icon.addPixmap(px);         //use our icon again
    _ui->markComboBox->addItem(icon,"red");

    color = settings.value("GraphView/Nodes/ColourBlue",
                           QColor(0x22,0x70,0xee) // blue
                           ).value<QColor>();
    px.fill(color);  //use our pixmap again, but with another color
    icon.addPixmap(px);         //use our icon again
    _ui->markComboBox->addItem(icon,"blue");

    color = settings.value("GraphView/Nodes/ColourGreen",
                           QColor(0x60,0xcb,0x60) // green
                           ).value<QColor>();
    px.fill(color);     //use our pixmap again, but with another color
    icon.addPixmap(px);             //use our icon again
    _ui->markComboBox->addItem(icon,"green");

    int currentMarkIndex = _ui->markComboBox->findText(_node->mark());
    if (currentMarkIndex != -1)
        _ui->markComboBox->setCurrentIndex(currentMarkIndex);
}

EditNodeDialog::~EditNodeDialog()
{
    delete _ui;
    delete _idValidator;
}

void EditNodeDialog::accept()
{
    _node->setId(_ui->idEdit->text());
    _node->setLabel(_ui->labelEdit->text());

    //qDebug() << "Node Label is: " << _ui->labelEdit->text();
    _node->setIsRoot(_ui->rootCheckBox->isChecked());
    //_node->setMarked(_ui->markedCheckBox->isChecked());
    _node->setMark(_ui->markComboBox->currentText());

    QDialog::accept();
}

}
