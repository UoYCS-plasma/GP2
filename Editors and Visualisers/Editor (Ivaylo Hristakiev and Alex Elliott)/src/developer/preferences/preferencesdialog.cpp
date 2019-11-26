/*!
 * \file
 */
#include "preferencesdialog.hpp"
#include "ui_preferencesdialog.h"

#include <QFile>
#include <QAbstractButton>

#include "projectpreferences.hpp"
#include "appearancepreferences.hpp"
#include "toolchainpreferences.hpp"

namespace Developer {

PreferencesDialog::PreferencesDialog(QWidget *parent)
    : QDialog(parent)
    , _ui(new Ui::PreferencesDialog)
{
    _ui->setupUi(this);

    // Load the main stylesheet and apply it to this widget
    QFile fp(":/stylesheets/preferences.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);

    ProjectPreferences *proj = new ProjectPreferences(this);
    _pages.push_back(proj);

    AppearancePreferences *app = new AppearancePreferences(this);
    _pages.push_back(app);

    ToolchainPreferences *toolchains = new ToolchainPreferences(this);
    _pages.push_back(toolchains);

    _ui->mainWidget->addTab(proj,
                            QIcon(QPixmap(":/icons/folder.png")),
                            tr("Projects")
                            );

    _ui->mainWidget->addTab(app,
                            QIcon(QPixmap(":/icons/palette.png")),
                            tr("Appearance")
                            );

    _ui->mainWidget->addTab(toolchains,
                            QIcon(QPixmap(":/icons/cog.png")),
                            tr("GP Toolchains")
                            );
}

PreferencesDialog::~PreferencesDialog()
{
    delete _ui;
}

void PreferencesDialog::reset()
{
    pageIter iter;
    for(iter = _pages.begin(); iter != _pages.end(); ++iter)
        (*iter)->reset();
}

void PreferencesDialog::apply()
{
    pageIter iter;
    for(iter = _pages.begin(); iter != _pages.end(); ++iter)
        (*iter)->apply();
}

void PreferencesDialog::buttonClicked(QAbstractButton *button)
{
    if(button->text() == QString("OK"))
        accept();
    if(button->text() == QString("Cancel"))
        reject();
    if(button->text() == QString("Reset"))
        reset();
    if(button->text() == QString("Apply"))
        apply();
}

void PreferencesDialog::accept()
{
    apply();
    QDialog::accept();
}

void PreferencesDialog::reject()
{
    QDialog::reject();
}

}
