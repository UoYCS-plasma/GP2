/*!
 * \file
 */
#ifndef NEWPROJECTWIZARD_HPP
#define NEWPROJECTWIZARD_HPP

#include <QWizard>
#include "project.hpp"

namespace Ui {
    class NewProjectWizard;
}

namespace Developer {

class NewProjectWizard : public QWizard
{
    Q_OBJECT
    
public:
    explicit NewProjectWizard(QWidget *parent = 0);
    ~NewProjectWizard();

    Project *project() const;

    bool validateCurrentPage();

public slots:
    void selectProjectLocation();
    void updateProjectLocation();
    void accept();
    
private:
    Ui::NewProjectWizard *_ui;
    QString _projectPath;
    Project *_project;
    QRegExp _fileNameValidation;
};

}

#endif // NEWPROJECTWIZARD_HPP
