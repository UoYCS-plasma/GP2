/*!
 * \file
 */
#include "prettytabheader.hpp"
#include "ui_prettytabheader.h"

namespace Developer {

PrettyTabHeader::PrettyTabHeader(const QString &title, QWidget *parent)
    : QWidget(parent)
    , _ui(new Ui::PrettyTabHeader)
{
    _ui->setupUi(this);
    _ui->label->setText(title);
}

PrettyTabHeader::~PrettyTabHeader()
{
    delete _ui;
}

}
