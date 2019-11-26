/*!
 * \file
 */
#ifndef PREFERENCESPAGE_HPP
#define PREFERENCESPAGE_HPP

#include <QWidget>
#include <QSettings>

namespace Developer {

class PreferencesPage : public QWidget
{
    Q_OBJECT

public:
    explicit PreferencesPage(QWidget *parent = 0);
    
    virtual void reset() = 0;
    virtual void apply() = 0;

protected:
    QSettings _settings;
};

}

#endif // PREFERENCESPAGE_HPP
