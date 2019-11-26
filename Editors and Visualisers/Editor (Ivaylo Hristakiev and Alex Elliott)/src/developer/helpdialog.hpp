/*!
 * \file
 */
#ifndef HELPDIALOG_HPP
#define HELPDIALOG_HPP

#include <QDialog>
#include <QTreeWidget>

namespace Ui {
    class HelpDialog;
}

namespace Developer {

class HelpDialog : public QDialog
{
    Q_OBJECT
    
public:
    /*!
     * Simple enum to make the process of inserting a new page into the help
     * dialog simpler, simply add a new enum value in the correct part of the
     * sequence and it will automatically map to the correct stackedWidget
     * index.
     */
    enum HelpPages
    {
        Introduction,
        Welcome,
        GeneralConcepts,
        GP,
        Rules,
        Programs,
        Injective
    };

    explicit HelpDialog(HelpPages initialPage = Introduction, QWidget *parent = 0);
    ~HelpDialog();

public slots:
    void helpItemClicked(QTreeWidgetItem *item);
    
private:
    Ui::HelpDialog *_ui;
};

}

#endif // HELPDIALOG_HPP
