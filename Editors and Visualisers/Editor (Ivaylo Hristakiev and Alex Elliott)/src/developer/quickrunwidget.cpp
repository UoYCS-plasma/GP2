/*!
 * \file
 */
#include "quickrunwidget.hpp"
#include "ui_quickrunwidget.h"

#include "project.hpp"

#include <QDebug>

namespace Developer {

QuickRunWidget::QuickRunWidget(QWidget *parent)
    : QWidget(parent)
    , _ui(new Ui::QuickRunWidget)
{
    _ui->setupUi(this);
}

QuickRunWidget::~QuickRunWidget()
{
    delete _ui;
}

void QuickRunWidget::setProject(Project *project)
{
    _ui->quickRunProjectName->setText(project->name());

    //! \todo iterate across run configurations, also need to track them
}

void QuickRunWidget::run()
{
    qDebug() << "Run requested: " << _ui->quickRunConfigCombo->currentText();
}

}
