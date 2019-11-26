/*!
 * \file
 */
#include "program.hpp"

#include <QDebug>
#include <QFileDialog>

namespace Developer {

Program::Program(const QString &programPath, QObject *parent)
    : GPFile(programPath, parent)
    , _name("")
    , _program("")
    , _documentation("")
{
    if(!programPath.isEmpty())
        open();
}

QString Program::name() const
{
    return _name;
}

void Program::setName(const QString &programName)
{
    _name = programName;
}

QString Program::program() const
{
    return _program;
}

void Program::setProgram(const QString &programText)
{
    if(_program == programText)
        return;

    _program = programText;
    _status = Modified;
    emit statusChanged(_status);
}

QString Program::documentation() const
{
    return _documentation;
}

void Program::setDocumentation(const QString &programDocumentation)
{
    if(programDocumentation == _documentation)
        return;

    _documentation = programDocumentation;
    _status = Modified;
    emit statusChanged(_status);
}

bool Program::save()
{
    // Some initial sanity checks
    if(_path.isEmpty() || !_fp->isOpen())
        return false;

    _fp->close();
    ++_internalChanges;
    _fp->open(QFile::Truncate | QFile::WriteOnly);
    qDebug() << "Saving program file: " << _fp->fileName();

		QString saveText = QString();

    // Construct the save file, this means making the documentation into a
    // comment and then concatenating the program contents
    QString docText = _documentation;
    docText.replace("\n","\n \\\\ ");
		if (!_documentation.isEmpty())
			saveText += QString("// ") + docText + QString("\n");

		saveText += _program ;

    ++_internalChanges;
    int status = _fp->write(QVariant(saveText).toByteArray());
    if(status <= 0)
    {
        qDebug() << "    Save failed";
        return false;
    }

    _fp->close();
    _fp->open(QFile::ReadWrite);

    qDebug() << "    Save completed. Wrote " << status << " bytes";

    _status = Normal;
    emit statusChanged(_status);
    return true;
}

bool Program::saveAs(const QString &filePath)
{
    QString thePath = filePath;
    if(filePath.isEmpty())
    {
        QDir d = dir();
        QString dirPath;
        if(d.path().isEmpty())
            dirPath = QDir::homePath();
        else
            dirPath = d.absolutePath();

        thePath = QFileDialog::getSaveFileName(
                    0,
                    tr("Save Program As..."),
                    dirPath,
                    tr("GP Programs (*.gpx)"));
        if(thePath.isEmpty())
            return false;
    }

    // Cache the path to the old file, if the save process fails then we should
    // restore the old one
    QString pathCache = _path;
    _path = thePath;
    open();
    if(!save())
    {
        // The save process failed
        qDebug() << "Program could not be saved to " << filePath;
        qDebug() << "Reopening previous file.";
        _path = pathCache;
        open();
        return false;
    }

    // Update the file watcher
    bool ret = GPFile::saveAs(_path);

    // Delete the old file as the move was successful
    QFile(pathCache).remove();

    return ret;
}

bool Program::open()
{
    if(!GPFile::open())
        return false;

    qDebug() << "Opening program file: " << absolutePath();

    setName(fileName());
    _program = _fp->readAll();

    //QRegExp rx("\\s*/\\*!(.*)\\*/");
    //rx.setMinimal(true);

    /*int pos = -1;
    if((pos = rx.indexIn(_program)) >= 0)
    {
        _program.remove(0, pos + rx.matchedLength());
        _program = _program.trimmed() + "\n";
        _documentation = rx.cap(1);
        rx = QRegExp("\n\\s*\\*\\s*");
        _documentation.replace(rx,"\n");
        _documentation = _documentation.trimmed();
    }*/

    qDebug() << "    Finished parsing program file: " << absolutePath();

    return true;
}

}
