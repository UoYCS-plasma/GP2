/*!
 * \file
 */
#ifndef WELCOME_HPP
#define WELCOME_HPP

#include <QWidget>
#include <QSignalMapper>

namespace Ui {
    class Welcome;
}

namespace Developer {

class Welcome : public QWidget
{
    Q_OBJECT
    
public:
    explicit Welcome(QWidget *parent = 0);
    ~Welcome();

public slots:
    void recentProjectsUpdated(QStringList projects);

signals:
    void newProjectClicked();
    void openProjectClicked();
    void openProjectClicked(QString);

private slots:
    void newProject();
    void openProject(QString path = QString());
    
private:
    Ui::Welcome *_ui;
    QSignalMapper *_mapper;
};

}

#endif // WELCOME_HPP
