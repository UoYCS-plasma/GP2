/*!
 * \file
 */
#ifndef PROJECTPREFERENCES_HPP
#define PROJECTPREFERENCES_HPP

#include "preferencespage.hpp"

namespace Ui {
    class ProjectPreferences;
}

namespace Developer {

class ProjectPreferences : public PreferencesPage
{
    Q_OBJECT
    
public:
    explicit ProjectPreferences(QWidget *parent = 0);
    ~ProjectPreferences();

    void reset();
    void apply();

public slots:
    void selectDefaultProjectLocation();
    
private:
    Ui::ProjectPreferences *_ui;
};

}

#endif // PROJECTPREFERENCES_HPP
