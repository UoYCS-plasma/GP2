/*!
 * \file
 */
#ifndef TOOLCHAINPREFERENCES_HPP
#define TOOLCHAINPREFERENCES_HPP

#include "preferencespage.hpp"

namespace Ui {
class ToolchainPreferences;
}

namespace Developer {

class ToolchainPreferences : public PreferencesPage
{
    Q_OBJECT
    
public:
    explicit ToolchainPreferences(QWidget *parent = 0);
    ~ToolchainPreferences();

    void reset();
    void apply();
    
private:
    Ui::ToolchainPreferences *_ui;
};

}

#endif // TOOLCHAINPREFERENCES_HPP
