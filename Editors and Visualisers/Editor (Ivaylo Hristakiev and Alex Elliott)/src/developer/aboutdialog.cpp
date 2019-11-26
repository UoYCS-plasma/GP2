/*!
 * \file
 */
#include "aboutdialog.hpp"
#include "ui_aboutdialog.h"

namespace Developer {

AboutDialog::AboutDialog(QWidget *parent)
    : QDialog(parent)
    , _ui(new Ui::AboutDialog)
{
    _ui->setupUi(this);
}

AboutDialog::~AboutDialog()
{
    delete _ui;
}

}
