/*!
 * \file
 */
#include "editedgedialog.hpp"
#include "ui_editedgedialog.h"

#include "graphview/edgeitem.hpp"
#include "listvalidator.hpp"
#include "edge.hpp"
#include "graph.hpp"

#include <QFile>
#include <QDebug>
#include <QSettings>

namespace Developer {

EditEdgeDialog::EditEdgeDialog(EdgeItem *edgeItem, QWidget *parent)
    : QDialog(parent)
    , _ui(new Ui::EditEdgeDialog)
    , _edge(edgeItem)
{
    _ui->setupUi(this);

    // Load the help stylesheet and apply it to this widget
    QFile fp(":/stylesheets/main.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);

    //QStringList nodes = _edge->edge()->parent()->nodeIdentifiers();

    _ui->idEdit->setText(_edge->id());
    _ui->idEdit->setReadOnly(true);
    _ui->bidirectionalBox->setChecked(_edge->isBidirectional());

    if (!_edge->edge()->parent()->isRuleGraph())
    {
        // If editing a host graph, disable ID display
        _ui->idEdit->setVisible(false);
        _ui->idLabel->setVisible(false);

        // and bidirectional selection
        _ui->bidirectionalBox->hide();

    }

    _ui->labelEdit->setText(_edge->label());

    // qDebug() << _edge->isBidirectional();

    //_ui->fromComboBox->addItems(nodes);
    //_ui->toComboBox->addItems(nodes);

    _ui->fromNode->setText(_edge->from()->id());
    //_ui->fromComboBox->setCurrentIndex(
    //            _ui->fromComboBox->findText(_edge->from()->id())
    //            );
    _ui->toNode->setText(_edge->to()->id());
    //_ui->toComboBox->setCurrentIndex(
    //            _ui->fromComboBox->findText(_edge->to()->id())
    //            );


    _ui->markComboBox->addItem("none");//set our icon

    QSettings settings;
    //create pixmap and choose size
    QPixmap px(15,15);
    QColor color;

    color = settings.value("GraphView/Edges/ColourAny",
                           QColor(0xee,0x82,0xee) // purple-ish
                           ).value<QColor>();
    px.fill(color);
    QIcon icon(px);


    if (_edge->edge()->parent()->isRuleGraph())
        // Only rule graphs have 'any'
        _ui->markComboBox->addItem(icon,"any");


    color = settings.value("GraphView/Edges/ColourShaded",
                           QColor(0xb2,0xb2,0xb2) // light gray
                           ).value<QColor>();
    px.fill(color);  //use our pixmap again, but with another color
    icon.addPixmap(px);         //use our icon again
    _ui->markComboBox->addItem(icon,"dashed");

    color = settings.value("GraphView/Edges/ColourRed",
                           QColor(0xee,0x77,0x77) // light red
                           ).value<QColor>();
    px.fill(color);  //use our pixmap again, but with another color
    icon.addPixmap(px);         //use our icon again
    _ui->markComboBox->addItem(icon,"red");

    color = settings.value("GraphView/Edges/ColourBlue",
                           QColor(0x22,0x70,0xee) // blue
                           ).value<QColor>();
    px.fill(color);  //use our pixmap again, but with another color
    icon.addPixmap(px);         //use our icon again
    _ui->markComboBox->addItem(icon,"blue");

    color = settings.value("GraphView/Edges/ColourGreen",
                           QColor(0x60,0xcb,0x60) // green
                           ).value<QColor>();
    px.fill(color);     //use our pixmap again, but with another color
    icon.addPixmap(px);             //use our icon again
    _ui->markComboBox->addItem(icon,"green");

    int currentMarkIndex = _ui->markComboBox->findText(_edge->mark());
    if (currentMarkIndex != -1)
        _ui->markComboBox->setCurrentIndex(currentMarkIndex);



    //_labelValidator = new ListValidator(this);
    //_ui->labelEdit->setValidator(_labelValidator);
}

EditEdgeDialog::~EditEdgeDialog()
{
    delete _ui;
}

void EditEdgeDialog::accept()
{
    // qDebug() << "Edge Label is: " << _ui->labelEdit->text();
    _edge->setLabel(_ui->labelEdit->text());
    _edge->setMark(_ui->markComboBox->currentText());
    _edge->setBidirectional(_ui->bidirectionalBox->isChecked());

    QDialog::accept();
}

}
