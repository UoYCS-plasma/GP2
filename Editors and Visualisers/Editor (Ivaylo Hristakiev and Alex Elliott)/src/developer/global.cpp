/*!
 * \file
 */
#include "global.hpp"

#include <QString>
#include <QDebug>

namespace Developer {

const QString GPVersionToString(GPVersions version)
{
    switch(version)
    {
        case GP2:
            return QString("gp2");
        default:
            qDebug() << "Unknown version passed: " << version;
            return QString("");
    }
}

GPVersions stringToGPVersion(const QString &version)
{
    if(version == "gp2")
        return GP2;

    qDebug() << "Unknown version string passed: " << version;
    qDebug() << "Defaulting to GP2";
    return GP2;
}

}
