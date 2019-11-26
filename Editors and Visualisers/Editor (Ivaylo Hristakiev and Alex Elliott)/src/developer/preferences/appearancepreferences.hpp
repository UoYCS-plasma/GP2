/*!
 * \file
 */
#ifndef APPEARANCEPREFERENCES_HPP
#define APPEARANCEPREFERENCES_HPP

#include "preferencespage.hpp"

namespace Ui {
class AppearancePreferences;
}

namespace Developer {

class AppearancePreferences : public PreferencesPage
{
    Q_OBJECT
    
public:
    explicit AppearancePreferences(QWidget *parent = 0);
    ~AppearancePreferences();

    void reset();
    void apply();
    
private:
    Ui::AppearancePreferences *_ui;
};

}

#endif // APPEARANCEPREFERENCES_HPP
