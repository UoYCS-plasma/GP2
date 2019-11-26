/*!
 * \file
 */
#include "gpfile.hpp"

#include <QStringList>
#include <QDebug>
#include <QFileInfo>

namespace Developer {

GPFile::GPFile(const QString &filePath, QObject *parent)
    : QObject(parent)
    , _path(filePath)
    , _fileWatcher(0)
    , _fp(0)
    , _status(GPFile::Modified)
    , _internalChanges(0)
{
//    qDebug () << "(9.06)";
    _fileWatcher = new QFileSystemWatcher(this);
    if (_fileWatcher == 0)
        qDebug () << "Warning: Could not create FileSystem Watcher";
//    qDebug () << "(9.08)";

//    qDebug () << "    gpfile.cpp:" << filePath;

    connect(_fileWatcher, SIGNAL(fileChanged(QString)),
            this, SLOT(fileChanged(QString)));

//    qDebug () << "(9.09)";
    if(!_path.isEmpty())
        open();
}

GPFile::~GPFile()
{
    // Only delete the file pointer if one has been created, in an unsaved
    // discard this may still be 0
    if(_fp != 0)
        delete _fp;
    delete _fileWatcher;
}

QString GPFile::path() const
{
    return _path;
}

QString GPFile::absolutePath() const
{
    QFileInfo f(_path);
    return f.absoluteFilePath();
}

QString GPFile::baseName() const
{
    if(_path.isEmpty())
        return QString();

    QFileInfo info(_path);
    if(!info.exists())
        return QString();

    return info.baseName();
}

QString GPFile::fileName() const
{
    if(_path.isEmpty())
        return QString();

    QFileInfo info(_path);
    if(!info.exists())
        return QString();

    return info.fileName();
}

QDir GPFile::dir() const
{
    if(_path.isEmpty())
        return QDir();

    QFile f(_path);
    if(!f.exists())
        return QDir();

    QFileInfo info(f);
    return info.dir();
}

GPFile::FileStatus GPFile::status() const
{
    return _status;
}

bool GPFile::saveAs(const QString &filePath)
{
    // This should be called with the new path
    if(filePath.isEmpty())
        return false;

    // The majority of the work in this function should be in the derived
    // classes, but we need to update the _fileWatcher regardless assuming that
    // the save process has worked without a failure

    // Clear the watcher of any existing path
    if(_fileWatcher->files().count() > 0)
        _fileWatcher->removePaths(_fileWatcher->files());
    _fileWatcher->addPath(filePath);

    // There's no reason for an error in this logic
    return true;
}

bool GPFile::open()
{
    // Set this to an error at the start, this will be changed if the open
    // succeeds
    _status = GPFile::Error;

//    qDebug () << "(9.10)";
    if(_fp != 0)
    {
        _fp->close();
        delete _fp;
    }

//    qDebug () << "(9.11)";
    // Clear the watcher of any existing path
    if(_fileWatcher->files().count() > 0)
        _fileWatcher->removePaths(_fileWatcher->files());

//    qDebug () << "(9.12)";
    _fp = new QFile(_path);
    // If this is a new file then we stick with GPFile::Modified as a status as
    // it needs to be saved or discarded, otherwise we stick with GPFile::Normal
    // - except in the case where we have been told to open a non-null path and
    // it does not exist, in which case we mark the file as GPFile::Deleted
    if(_fp->exists() && !_path.startsWith(":"))
    {
        //_status = GPFile::Normal;
        // It exists, so we should watch it
        _fileWatcher->addPath(_path);
//        qDebug () << "(9.13)";
    }
    else if(!_path.startsWith(":"))
    {
        // It doesn't exist, is this a new file, or a missing one?
        if(_path.isEmpty())
            _status = GPFile::Modified;
        else
            _status = GPFile::Deleted;
    }
    else
    {
        // The file is an internal one, probably from the :templates/ resource.
        // Mark it as read-only as we don't intend for people to be able to
        // write there
        _status = GPFile::ReadOnly;
    }

    QFileInfo info(_path);
    bool readOnly = ((info.exists() && !info.isWritable()) || _path.startsWith(":"));

    if(!readOnly && _fp->open(QFile::ReadWrite))
    {
        // We opened it fine, if we are still using the initial error value then
        // change it here
        if(_status == GPFile::Error)
            _status = GPFile::Normal;
        emit statusChanged(_status);
        return true;
    }
    else if(readOnly && _fp->open(QFile::ReadOnly | QFile::Text))
    {
        if(_status == GPFile::Error)
            _status = GPFile::ReadOnly;
        emit statusChanged(_status);
        return true;
    }
    else
    {
        qDebug() << "Unknown error while opening file: " << _path;
        emit statusChanged(_status);
        return false;
    }
}

void GPFile::fileChanged(const QString &filePath)
{
    if(_internalChanges > 0)
    {
        --_internalChanges;
        return;
    }

    if(filePath != _path)
    {
        qDebug() << "File path mismatch:";
        qDebug() << "  _path = " << _path;
        qDebug() << "  filePath = " << filePath;
    }

    //NOTE:
    // This might need logic for when a change is made within the application
    // The status should only change for external edits
    _status = GPFile::ExternallyModified;
    emit statusChanged(_status);
}

}
