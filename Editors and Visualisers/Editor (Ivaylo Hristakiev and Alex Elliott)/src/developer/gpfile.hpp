/*!
 * \file
 */
#ifndef GPFILE_HPP
#define GPFILE_HPP

#include <QObject>
#include <QString>
#include <QFile>
#include <QFileSystemWatcher>
#include <QDir>

#include "global.hpp"

namespace Developer {

/*!
 * \brief Abstract base class for GP project files
 *
 * The GPFile class provides a generalised interface for querying and changing
 * the state of files in a GP project. The class maintains a FileStatus member
 * variable which informs the rest of the system whether the file needs to be
 * saved, reloaded, etc.
 *
 * In addition it provides an interface for those tasks. The save() function
 * saves any pending changes to the file contained in _path (or prompts the user
 * for a location if one is not set), and saveAs() saves the file to a new
 * location given by its argument.
 */
class GPFile : public QObject
{
    Q_OBJECT

public:
    /*!
     * \brief Enumeration covering the possible states of a file in the system
     */
    enum FileStatus
    {
        //! Normal, no changes
        Normal,
        //! Read-only, cannot be modified
        ReadOnly,
        //! File has been modified
        Modified,
        //! File has been modified externally
        ExternallyModified,
        //! File has been deleted / file not found
        Deleted,

        //! Error, unknown issue
        Error
    };

    /*!
     * \brief Construct a new GPFile with the given file path if provided
     * \param filePath  The path to this file on the filesystem
     * \param parent    This object's parent object
     */
    GPFile(const QString &filePath = QString(), QObject *parent = 0);

    /*!
     * \brief Destroy the GPFile object and release held memory
     */
    virtual ~GPFile();

    /*!
     * \brief Get the current path of the active file
     * \return A string containing the path to the current file on the
     *  filesystem
     */
    QString path() const;

    /*!
     * \brief Get the absolute path of this file
     * \return A string containing an absolute path to this file on the
     *  filesystem
     */
    QString absolutePath() const;

    /*!
     * \brief Get the base name of this file (file name without extension)
     *
     * If the file has not been created then this member function returns an
     * empty string.
     *
     * \return A string containing the basename of this file if it exists or an
     *  empty string otherwise
     */
    QString baseName() const;

    /*!
     * \brief Get the filename of this file if it exists
     *
     * If the file has not been created then this member function returns an
     * empty string.
     *
     * \return A string containing the filename of this file if it exists or an
     *  empty string otherwise
     */
    QString fileName() const;

    /*!
     * \brief Get the directory in which this file sits
     * \return A QDir representing the directory of this file or a null QDir if
     *  the file does not exist yet
     */
    QDir dir() const;

    /*!
     * \brief Get the current status of this file
     * \return The status of this file
     * \sa FileStatus
     */
    FileStatus status() const;

    /*!
     * \brief Save this file to the current active file
     *
     * If there is no current active file (in the case of a new unsaved file)
     * then prompt the user for a location to save this file.
     *
     * \return Boolean, true if saved successfully, false otherwise
     */
    virtual bool save() = 0;

    /*!
     * \brief Save this file to a new location
     *
     * The majority of the functionality of this function will be in the
     * concrete derived classes, but those methods should finish by returning
     * GPFile::saveAs(filePath) to ensure that the file watcher remains correct.
     *
     * \param filePath  The new path to save this file to
     * \return Boolean, true if saved successfully, false otherwise
     */
    virtual bool saveAs(const QString &filePath = QString());

    /*!
     * \brief Open the file into the protected variable _fp
     *
     * This sets up everything that is generic to all files, it is intended that
     * this method should be overridden in derived classes, which will initial
     * call this function via GPFile::open() and then perform logic specific to
     * the type of file opened.
     *
     * \return Boolean, true if opened successfully, false otherwise
     */
    virtual bool open();

signals:
    /*!
     * \brief This signal is emitted whenever it is detected that the file has
     *  changed in any way.
     *
     * This change can be triggered by normal editing within the IDE, edits made
     * outside of the running editor or the file being deleted from the
     * filesystem.
     *
     * In cases where more than one applies the order of precedence should be
     *
     *-# Error
     *-# Deleted
     *-# ExternallyModified
     *-# Modified
     *-# Normal
     *
     * \param status The new status of the file
     */
    void statusChanged(FileStatus status);

protected slots:
    /*!
     * \brief This slot handles a detected change made to the file
     * \param filePath The path of the file edited, this should match the
     *  contents of _path
     */
    void fileChanged(const QString &filePath);

protected:
    /*!
     * \brief This is the path to the file currently open
     */
    QString _path;

    /*!
     * \brief This internal variable contains the watcher which ensures that
     *  the file status remains up-to-date
     */
    QFileSystemWatcher *_fileWatcher;

    /*!
     * \brief An internal file pointer to the given path
     */
    QFile *_fp;

    /*!
     * \brief The file's current status
     */
    FileStatus _status;

    int _internalChanges;
};

}

#endif // GPFILE_HPP
