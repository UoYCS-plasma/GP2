/*!
 * \file
 */
#ifndef ABOUTDIALOG_HPP
#define ABOUTDIALOG_HPP

#include <QDialog>

namespace Ui {
    class AboutDialog;
}

namespace Developer {

/*!
 * \brief Presents a simple "About" dialog which describes what the application
 *  uses
 *
 * This is a very simplistic UI stub which presents a static dialog with QLabel
 * contents and a standard accept button.
 */
class AboutDialog : public QDialog
{
    Q_OBJECT
    
public:
    /*!
     * \brief Construct a new AboutDialog
     * \param parent This widget's parent widget (MainWindow)
     */
    explicit AboutDialog(QWidget *parent = 0);

    /*!
     * \brief Destroy this dialog and release the UI's allocation
     */
    ~AboutDialog();
    
private:
    Ui::AboutDialog *_ui;
};

}

#endif // ABOUTDIALOG_HPP
