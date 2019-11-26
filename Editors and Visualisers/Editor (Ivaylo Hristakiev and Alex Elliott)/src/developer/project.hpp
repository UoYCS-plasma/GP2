/*!
 * \file
 */
#ifndef PROJECT_HPP
#define PROJECT_HPP

#include "graph.hpp"
#include "program.hpp"
#include "rule.hpp"
#include "runconfig.hpp"

#include <QVector>
#include <QDebug>
#include <QDomNode>

namespace Developer {

class OpenThread;

/*!
 * \brief Container type for GP projects, allowing for monitoring and updating
 *      project files.
 *
 * The general structure of projects is as follows:
 *
 * \code
 *  Project:
 *      - project file: (.gpp)
 *          - version information
 *          - rule/program/graph locations
 *          - run profiles
 *      - rules:
 *          - rule files (.gpr)
 *      - programs:
 *          - program files (.gpx)
 *      - graphs
 *          - graph files (.gv, .gxl, .gpg)
 * \endcode
 *
 * An example of a simple project file follows, the format is a simple dialect
 * of XML containing the necessary information to locate all of the project's
 * files.
 *
 * \code
 *  <?xml version="1.0" encoding="UTF-8" ?>
 *  <project name="Project1" directory="/home/user/gpdeveloper/project1">
 *      <rules>
 *          <rule>rule1.gpr</rule>
 *          <rule>rule2.gpr</rule>
 *          <rule>rule3.gpr</rule>
 *      </rules>
 *      <programs>
 *          <program>program1.gpx</program>
 *      </programs>
 *      <graphs>
 *          <graph>graph1.gxl</graph>
 *          <graph>graph2.gv</graph>
 *      </graphs>
 *      <runconfigurations>
 *          <runconfiguration name="run1" program="program1" graph="graph1.gxl" />
 *      </runconfigurations>
 *  </project>
 * \endcode
 *
 * The Project class does not expect the user to manually save the file at any
 * point, and therefore is handled differently than other GPFile derived
 * classes. The file's status, and the statusChanged signals are largely unused,
 * and save() is called after any changes are detected in order to ensure that
 * the project file remains up to date (and therefore that the _status remains
 * Normal) at all times.
 */
class Project : public GPFile
{
    Q_OBJECT

public:
    /*!
     * \brief The FileTypes enum specifies the types of file the project can
     *  contain
     */
    enum FileTypes
    {
        //! The project (.gpp) file
        ProjectFile,
        //! Any GP rule (.gpr) files
        RuleFile,
        //! Any GP program (.gpx) files
        ProgramFile,
        //! Any graph (.gv or .gxl) files
        GraphFile
    };

    /*!
     * \brief Construct a new Project
     *
     * If a path to a valid project file is found then initialise using that
     * file, otherwise create a blank project which then must be initialised
     * with initProject() before the project can be used.
     *
     * \param projectPath       Optional path to an existing project file
     * \param autoInitialise    Optional. If passed "false" then the project
     *      object will not read in and parse the project file itself.
     * \param parent            This object's parent object
     */
    Project(const QString &projectPath = QString(), bool autoInitialise = true, QObject *parent = 0);

    /*!
     * \brief Destroy the project object and free memory
     */
    ~Project();

    /*!
     * \brief Return the name of this project
     *
     * \return The project's name
     */
    QString name() const;

    /*!
     * \brief Set the name of this project with the provided string
     *
     * This will not change the project's path, it will only update the value
     * contained within the project.
     *
     * \param name  The new name of this project
     */
    void setName(const QString &name);

    /*!
     * \brief Return the GP version this project uses
     *
     * \return The GP version of this project
     */
    GPVersions gpVersion() const;

    /*!
     * \brief Change the version of GP this project uses
     *
     * This should only be used between similar enough variants that this
     * transition makes sense. For example a non-rooted GP2 might be upgraded to
     * include those semantics - or in the future separate macros might be added
     * and extension to that version could be possible.
     *
     * In general however this should only move forwards, updating the version
     * to a previous version which does not support any of the features used
     * will lead to compile-time errors or lost information.
     *
     * \param version The version of GP this project should now use
     */
    void setGPVersion(GPVersions version);

    /*!
     * \brief Tests whether this is a null Project or not
     *
     * When a Project is constructed it will automatically try to initialise
     * itself if it is passed a project path. If nothing is passed, or if the
     * initialisation process fails somehow then isNull() will return true in
     * order to allow the developer to handle this failure.
     *
     * This should be a mandatory test whenever you construct a new Project.
     *
     * \return Boolean, false if the Project has opened successfully and is
     *  valid, true otherwise
     */
    bool isNull() const;

    /*!
     * \brief Return the last error encountered
     *
     * Whenever an operation occurs which might fail the functions return a
     * simple boolean value to indicate success or failure. In the case where a
     * member function returns false this function should contain a string
     * describing the failure which occurred.
     *
     * If the last operation was successful this function should return an empty
     * QString.
     *
     * \return A string containing the last error encountered or an empty string
     *  if the last operation was successful
     */
    QString error() const;

    /*!
     * \brief Return the directory path to the project's "graphs" subdirectory
     *
     * In the case where this directory has been deleted for whatever reason it
     * is created again.
     *
     * \return A QDir object containing the graphs subdirectory if the project
     *  exists, returns a null QDir if it doesn't exist yet
     */
    QDir graphsDir() const;

    /*!
     * \brief Return the directory path to the project's "programs" subdirectory
     *
     * In the case where this directory has been deleted for whatever reason it
     * is created again.
     *
     * \return A QDir object containing the programs subdirectory if the project
     *  exists, returns a null QDir if it doesn't exist yet
     */
    QDir programsDir() const;

    /*!
     * \brief Return the directory path to the project's "rules" subdirectory
     *
     * In the case where this directory has been deleted for whatever reason it
     * is created again.
     *
     * \return A QDir object containing the rules subdirectory if the project
     *  exists, returns a null QDir if it doesn't exist yet
     */
    QDir rulesDir() const;


    /*!
     * \brief Return the directory path to the project's "results" subdirectory
     *
     * In the case where this directory has been deleted for whatever reason it
     * is created again.
     *
     * \return A QDir object containing the results subdirectory if the project
     *  exists, returns a null QDir if it doesn't exist yet
     */
    QDir resultsDir() const;

    /*!
     * \brief Get a GPFile object for the provided file path if the project
     *  tracks it
     * \param filePath The file to search for in the project
     * \return A GPFile representing the requested file if found, 0 otherwise
     */
    GPFile *file(const QString &filePath) const;
    /*!
     * \brief Get a Rule object for the provided file path if the project tracks
     *  it
     * \param filePath The file to search for in the project
     * \return A Rule representing the requested file if found, 0 otherwise
     */
    Rule *rule(const QString &filePath) const;
    /*!
     * \brief Get a Program object for the provided file path if the project
     *  tracks it
     * \param filePath The file to search for in the project
     * \return A Program representing the requested file if found, 0 otherwise
     */
    Program *program(const QString &filePath) const;
    /*!
     * \brief Get a Graph object for the provided file path if the project
     *  tracks it
     * \param filePath The file to search for in the project
     * \return A Graph representing the requested file if found, 0 otherwise
     */
    Graph *graph(const QString &filePath) const;

    /*!
     * \brief Retrieve a vector of rule objects containing all of the rule files
     *  tracked by this project
     * \return A vector of Rule objects tracked by this project
     */
    QVector<Rule *> rules() const;
    /*!
     * \brief Retrieve a vector of program objects containing all of the program
     *  files tracked by this project
     * \return A vector of Program objects tracked by this project
     */
    QVector<Program *> programs() const;
    /*!
     * \brief Retrieve a vector of graph objects containing all of the graph
     *  files tracked by this project
     * \return A vector of Rule objects tracked by this project
     */
    QVector<Graph *> graphs() const;


    /*!
     * \brief Retrieve a vector of run config objects containing all of the run
     *  configurations tracked by this project
     * \return A vector of RunConfig objects tracked by this project
     */
    QVector<RunConfig *> runConfigurations() const;

    /*!
     * \brief Checks if the project has any unsaved changes stored
     * \return True if there are unsaved changes, false otherwise
     */
    bool hasUnsavedChanges() const;

    // This is used as in the parent class, no need to repeat the documentation
    bool open();

    /*!
     * \brief Open an existing project file at the provided location
     *
     * This member function contains the extended semantics involved with
     * opening a project file (namely parsing that file into a set of files and
     * run configurations).
     *
     * \param projectPath   The path to the project file
     * \return True if successfully opened, false otherwise
     */
    bool open(const QString &projectPath);

    bool readRules(QDomNode &node);
    bool readPrograms(QDomNode &node);
    bool readGraphs(QDomNode &node);
    bool readRunConfigs(QDomNode &node);

    /*!
     * brief Initialise a new project at the target location
     *
     * A blank project file is created along with subdirectories for rules,
     * programs and graphs.
     *
     * \param targetPath    The path the project should be created at
     * \param projectName   The initial name for this project
     * \param gpVersion     The version of GP this project uses
     * \return  Returns true if the project was successfully created, false if
     *  it was not or if the user cancelled
     */
    bool initProject(const QString &targetPath, const QString &projectName, GPVersions gpVersion);

    // Set of methods to create new (empty) files
    /*!
     * \brief Create a new GP rule for this project
     *
     * The rule created will be empty and will be automatically added to the
     * project file.
     *
     * \param name  The name of the new GP rule to create
     */
    void newRule(const QString &ruleName = QString());

    /*!
     * \brief Create a new GP program for this project
     *
     * The program created will be empty and will be automatically added to the
     * project file.
     *
     * \param name  The name of the new GP program to create
     */
    void newProgram(const QString &programName = QString());

    /*!
     * \brief Create a new blank graph file
     *
     * The filetype of the graph can be specified using the second parameter and
     * the produced file will be automatically added to the project file.
     *
     * \param name  The name of the new graph to create
     * \param type  The type of graph to create
     */
    void newGraph(const QString &graphName = QString(), GraphTypes type = DefaultGraph);

    // Set of methods to add files to the current tracked project
    /*!
     * \brief Add an existing rule to the current project
     *
     * This file does not have to be within the current project's source tree.
     * If the file is not then the system will offer to copy it into the
     * project's directory - but if the user declines then the file will be
     * added with an absolute path.
     *
     * \param filePath  The path to the new file to add to the project
     */
    void addRule(const QString &filePath);

    /*!
     * \brief Add an existing program to the current project
     *
     * This file does not have to be within the current project's source tree.
     * If the file is not then the system will offer to copy it into the
     * project's directory - but if the user declines then the file will be
     * added with an absolute path.
     *
     * \param filePath  The path to the new file to add to the project
     */
    void addProgram(const QString &filePath);

    /*!
     * \brief Add an existing graph to the current project
     *
     * This file does not have to be within the current project's source tree.
     * If the file is not then the system will offer to copy it into the
     * project's directory - but if the user declines then the file will be
     * added with an absolute path.
     *
     * This method can transparently handle GXL or Dot format graphs.
     *
     * \param filePath  The path to the new file to add to the project
     */
    void addGraph(const QString &filePath);

    /*!
     * \brief Add an existing Run Configuration to the current project
     *
     * \param filePath  The RunConfig object to be added
     */
    bool addRunConfig(RunConfig *runConfig);
    void removeConfig(RunConfig *runConfig);

    /*!
     * \brief Boolean test to determine if the project tracks the file provided
     * \param filePath  The file to test the presence of
     * \return Boolean, true if the project contains that file, false otherwise
     */
    bool containsFile(const QString &filePath);
    /*!
     * \brief Boolean test to determine if the project tracks the rule provided
     *
     * The test explicitly only considers those files considered rules and
     * ignores other files tracked by this project.
     *
     * \param filePath  The file to test the presence of
     * \return Boolean, true if the project contains that file, false otherwise
     */
    bool containsRule(const QString &filePath);
    /*!
     * \brief Boolean test to determine if the project tracks the program
     *  provided
     *
     * The test explicitly only considers those files considered programs and
     * ignores other files tracked by this project.
     *
     * \param filePath  The file to test the presence of
     * \return Boolean, true if the project contains that file, false otherwise
     */
    bool containsProgram(const QString &filePath);
    /*!
     * \brief Boolean test to determine if the project tracks the graph provided
     *
     * The test explicitly only considers those files considered graphs and
     * ignores other files tracked by this project.
     *
     * \param filePath  The file to test the presence of
     * \return Boolean, true if the project contains that file, false otherwise
     */
    bool containsGraph(const QString &filePath);

    /*!
     * \brief Boolean test to determine if the project tracks the run configuration provided
     *
     * The test explicitly only considers those objects considered run configurations and
     * ignores other objects tracked by this project.
     *
     * \param runConfig  The run configuration name to test the presence of
     * \return Boolean, true if the project contains that run configuration, false otherwise
     */
    bool containsRunConfig(RunConfig* config);
    bool containsRunConfigName(QString runConfigName);

    RunConfig* runConfig(QString &configName);

    // Inherited methods from GPFile
    bool save();
    bool saveAs(const QString &filePath);

public slots:
    /*!
     * \brief Inform the project of which file is currently active in the IDE
     *
     * Methods like save() will use this information when determining which file
     * should be saved when that action is requested by the user.
     *
     * \param fileName  The stored path of the new active file
     * \param type      The type of file to speed up location (rule, program,
     *  graph)
     */
    void setCurrentFile(const QString &fileName, FileTypes type);

    /*!
     * \brief Save the file specified
     * \param file  The path to the file
     * \return Boolean, true if saved successfully, false otherwise
     */
    bool saveFile(QString filePath = QString());
    bool saveCurrentFile();

    /*!
     * \brief Save the file specified to a new location
     * \param file  The path to the file
     * \return Boolean, true if saved successfully, false otherwise
     */
    bool saveFileAs(const QString &filePath = QString());
    bool saveCurrentFileAs();

    /*!
     * \brief Save all of the modified files tracked by this project
     * \return Boolean, true if saved successfully, false otherwise
     */
    bool saveAll();

    void exec();

signals:
    // Signals when a file within this project changes
    /*!
     * \brief Signal sent out whenever one of the files tracked in this project
     *  is changed
     * \param filePath The (absolute) path to the changed file
     */
    void fileChanged(QString filePath);
    /*!
     * \brief Signal sent out whenever one of the rules tracked in this project
     *  is changed
     * \param filePath The (absolute) path to the changed file
     */
    void ruleChanged(QString filePath);
    /*!
     * \brief Signal sent out whenever one of the programs tracked in this
     *  project is changed
     * \param filePath The (absolute) path to the changed file
     */
    void programChanged(QString filePath);
    /*!
     * \brief Signal sent out whenever one of the graphs tracked in this project
     *  is changed
     * \param filePath The (absolute) path to the changed file
     */
    void graphChanged(QString filePath);
    /*!
     * \brief Signal sent out whenever one of the run configurations stored
     *  in this project is changed
     * \param configurationName The name of the run configuration which has been
     *  altered
     */
    void runConfigurationChanged(QString configurationName);

    // Signals for when a file is added/deleted
    /*!
     * \brief Signal sent out whenever the list of files this project tracks is
     *  changed
     */
    void fileListChanged();
    /*!
     * \brief Signal sent out whenever the list of rules this project tracks is
     *  changed
     */
    void ruleListChanged();
    /*!
     * \brief Signal sent out whenever the list of programs this project tracks
     *  is changed
     */
    void programListChanged();
    /*!
     * \brief Signal sent out whenever the list of graphs this project tracks is
     *  changed
     */
    void graphListChanged();
    /*!
     * \brief Signal sent out whenever the list of run configurations stored in
     *  this project is changed
     */
    void runConfigurationListChanged();

    void fileStatusChanged(QString filePath, int status);
    void ruleStatusChanged(QString filePath, int status);
    void programStatusChanged(QString filePath, int status);
    void graphStatusChanged(QString filePath, int status);

    /*!
     * \brief Signal emitted when the current file of the project has been
     *  changed
     * \param current The file which is the new active file
     */
    void currentFileChanged(GPFile *current);

    void nodeCountChanged(int count);
    void edgeCountChanged(int count);
    void openComplete();

private slots:
    /*!
     * Internal slot used to handle file update signals from the directory
     * watcher instance. Determines if the file modified is tracked and if it is
     * then a signal is emitted to pass this change on to the application.
     *
     * \param filePath  The file which has changed
     */
    void fileModified(QString filePath);

    void trackRuleStatusChange(FileStatus status);
    void trackProgramStatusChange(FileStatus status);
    void trackGraphStatusChange(FileStatus status);

    void incrementNodeCount();
    void incrementEdgeCount();

private:
    /*!
     * Status variables for the current project to simplify data input/output
     * from Project objects
     */
    GPVersions _gpVersion;
    double _gpDeveloperVersion;
    QString _name;
    bool _null;
    // These are cosmetic for the opening of large projects
    int _nodeCount;
    int _edgeCount;


	void readRunConfigOptions(QDomNode &node, RunConfig* config);

    /*!
     * Error string which contains the last error encountered for elaboration
     * after returning false
     */
    QString _error;

    GPFile *_currentFile;

    QVector<Rule *> _rules;
    QVector<Graph *> _graphs;
    QVector<Program *> _programs;
    QVector<RunConfig *> _runConfigurations;

    // Set of convenience typedefs (don't want to rely on auto just yet)
    typedef QVector<Rule *>::iterator ruleIter;
    typedef QVector<Graph *>::iterator graphIter;
    typedef QVector<Program *>::iterator programIter;
    typedef QVector<RunConfig *>::iterator runConfigIter;
    typedef QVector<Rule *>::const_iterator ruleConstIter;
    typedef QVector<Graph *>::const_iterator graphConstIter;
    typedef QVector<Program *>::const_iterator programConstIter;
    typedef QVector<RunConfig *>::const_iterator runConfigConstIter;
};

}

#endif // PROJECT_HPP
