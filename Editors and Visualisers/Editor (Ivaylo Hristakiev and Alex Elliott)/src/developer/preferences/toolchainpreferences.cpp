/*!
 * \file
 */
#include "toolchainpreferences.hpp"
#include "ui_toolchainpreferences.h"

namespace Developer {

ToolchainPreferences::ToolchainPreferences(QWidget *parent)
    : PreferencesPage(parent)
    , _ui(new Ui::ToolchainPreferences)
{
    _ui->setupUi(this);
}

ToolchainPreferences::~ToolchainPreferences()
{
    delete _ui;
}

void ToolchainPreferences::reset()
{
    QSettings settings;
}

void ToolchainPreferences::apply()
{
    QSettings settings;
}

}
