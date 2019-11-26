/*!
 * \file
 */
#include <QApplication>
#include "mainwindow.hpp"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    a.setOrganizationName("UoYCS");
    a.setOrganizationDomain("www.cs.york.ac.uk");
    a.setApplicationName("GP Developer");

    qDebug () << "Starting application.";

    Developer::MainWindow w;
    w.show();

    
    return a.exec();
}
