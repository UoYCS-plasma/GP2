/*!
 * \file
 */
#include "project.hpp"

#include <QMessageBox>
#include <QDateTime>

#include <QDomDocument>
#include <QFileDialog>

namespace Developer {

Project::Project(const QString &projectPath, bool autoInitialise, QObject *parent)
    : GPFile(projectPath, parent)
    , _gpVersion(DEFAULT_GP_VERSION)
    , _gpDeveloperVersion(GP_DEVELOPER_VERSION)
    , _name("")
    , _null(true)
    , _nodeCount(0)
    , _edgeCount(0)
    , _error("")
{
    if(!projectPath.isEmpty() && autoInitialise)
       if  (!open(projectPath))
            qDebug () << "Error: Opening" << projectPath << "failed.";
    else
    {
        if(!projectPath.isEmpty())
        {
            // We've been given a path but told not to do a complete set-up,
            // this is used to set up name() and absolutePath() without reading
            // in the whole file.
            setName(fileName());
        }
        else
        {
//            qDebug () << "Error: Opening" << projectPath << "failed.";
        }
    }
}

Project::~Project()
{
    // delete child file objects (rules, programs, graphs, run configs)
    for(ruleIter iter = _rules.begin(); iter != _rules.end(); ++iter)
        delete *iter;
    for(programIter iter = _programs.begin(); iter != _programs.end(); ++iter)
        delete *iter;
    for(graphIter iter = _graphs.begin(); iter != _graphs.end(); ++iter)
        delete *iter;
    for(runConfigIter iter = _runConfigurations.begin(); iter != _runConfigurations.end(); ++iter)
        delete *iter;

//    if (_currentFile != 0)
//        delete _currentFile;
}

QString Project::name() const
{
    return _name;
}

void Project::setName(const QString &name)
{
    _name = name;
}

GPVersions Project::gpVersion() const
{
    return _gpVersion;
}

void Project::setGPVersion(GPVersions version)
{
    _gpVersion = version;
}

bool Project::isNull() const
{
    return _null;
}

QString Project::error() const
{
    return _error;
}

QDir Project::rulesDir() const
{
    QDir d = dir();
    d.cd("rules");

    // This directory should always exist, if it doesn't then make it again
    if(!d.exists())
        d.mkpath(d.absolutePath());

    return d;
}


QDir Project::resultsDir() const
{
    QDir d = dir();
    d.cd("results");

    // This directory should always exist, if it doesn't then make it again
    if(!d.exists())
        { d.mkpath(d.absolutePath()); qDebug() << "Directory doesn't exist: " << d.absolutePath(); }

    return d;
}

QDir Project::programsDir() const
{
    QDir d = dir();
    d.cd("programs");

    // This directory should always exist, if it doesn't then make it again
    if(!d.exists())
        d.mkpath(d.absolutePath());

    return d;
}

QDir Project::graphsDir() const
{
    QDir d = dir();
    d.cd("graphs");

    // This directory should always exist, if it doesn't then make it again
    if(!d.exists())
        d.mkpath(d.absolutePath());

    return d;
}

GPFile *Project::file(const QString &filePath) const
{
    Rule *r = rule(filePath);
    if(r != 0)
        return r;

    Program *p = program(filePath);
    if(p != 0)
        return p;

    Graph *g = graph(filePath);
    if(g != 0)
        return g;

    // We haven't found it, return 0
    return 0;
}

Rule *Project::rule(const QString &filePath) const
{
    // Do an initial check based on names
    for(ruleConstIter iter = _rules.begin(); iter != _rules.end(); ++iter)
    {
        Rule *r = *iter;
        if(r->name() == filePath)
            return r;
    }

    QString rulePath = filePath;
    QFileInfo i(filePath);
    if(!i.exists())
    {
        // Were we given an absolute path that doesn't exist? If yes, bail
        if(i.isAbsolute())
        {
            qDebug() << "The rule at the provided path could not be found: "
                     << filePath;
            return 0;
        }

        // The file doesn't exist, maybe we were just passed in a name - if so
        // then we need to fix it. First does it end with the right extension?
        if(!filePath.endsWith(GP_RULE_EXTENSION))
            rulePath += GP_RULE_EXTENSION;

        i.setFile(rulePath);
        // If it still doesn't exist then we'll try putting it in the programs/
        // subdirectory
        if(!i.exists())
        {
            rulePath = rulesDir().filePath(rulePath);

            // Still no? Then we're out of ideas
            i.setFile(rulePath);
            if(!i.exists())
                return 0;
        }
    }

    // The file does exist, so check it against the ones we have stored
    for(ruleConstIter iter = _rules.begin(); iter != _rules.end(); ++iter)
    {
        Rule *r = *iter;
        QFileInfo info(rulePath);
        if(r->absolutePath() == info.absoluteFilePath())
            return r;
    }

    // We haven't found it, return 0
    return 0;
}

Program *Project::program(const QString &filePath) const
{
    // Do an initial check based on names
    for(programConstIter iter = _programs.begin(); iter != _programs.end();
        ++iter)
    {
        Program *p = *iter;
        if(p->name() == filePath)
            return p;
    }

    QString programPath = filePath;
    QFileInfo i(filePath);
    if(!i.exists())
    {
        // Were we given an absolute path that doesn't exist? If yes, bail
        if(i.isAbsolute())
        {
            qDebug() << "The program at the path provided could not be found: "
                     << filePath;
            return 0;
        }

        // The file doesn't exist, maybe we were just passed in a name - if so
        // then we need to fix it. First does it end with the right extension?

        ProgramTypes type = DefaultProgram;
        if(filePath.endsWith(GP_OLD_EXTENSION))
            type = ProgramOldFormat;
        if(filePath.endsWith(GP_PROGRAM_EXTENSION))
            type = ProgramGP2Format;

        if (type == DefaultProgram)
            programPath += GP_PROGRAM_DEFAULT_EXTENSION;

        i.setFile(programPath);
        // If it still doesn't exist then we'll try putting it in the programs/
        // subdirectory
        if(!i.exists())
        {
            programPath = programsDir().filePath(programPath);

            // Still no? Then we're out of ideas
            i.setFile(programPath);
            if(!i.exists())
                return 0;
        }
    }

    // The file does exist, so check it against the ones we have stored
    for(programConstIter iter = _programs.begin(); iter != _programs.end();
        ++iter)
    {
        Program *p = *iter;
        QFileInfo info(programPath);
        if(p->absolutePath() == info.absoluteFilePath())
            return p;
    }

    // We haven't found it, return 0
    return 0;
}

Graph *Project::graph(const QString &filePath) const
{
    QFileInfo info(filePath);
    for(graphConstIter iter = _graphs.begin(); iter != _graphs.end(); ++iter)
    {
        Graph *g = *iter;
        if(g->absolutePath() == info.absoluteFilePath())
            return g;
        if(g->fileName() == filePath)
            return g;
    }

    // We haven't found it, return 0
    return 0;
}

QVector<Rule *> Project::rules() const
{
    return _rules;
}

QVector<Program *> Project::programs() const
{
    return _programs;
}

QVector<Graph *> Project::graphs() const
{
    return _graphs;
}

QVector<RunConfig *> Project::runConfigurations() const
{
    return _runConfigurations;
}

bool Project::hasUnsavedChanges() const
{
    for(ruleConstIter iter = _rules.begin(); iter != _rules.end(); ++iter)
    {
        Rule *r = *iter;
        if(r->status() == Modified)
            return true;
    }

    for(programConstIter iter = _programs.begin(); iter != _programs.end();
        ++iter)
    {
        Program *p = *iter;
        if(p->status() == Modified)
            return true;
    }

    for(graphConstIter iter = _graphs.begin(); iter != _graphs.end(); ++iter)
    {
        Graph *g = *iter;
        if(g->status() == Modified)
            return true;
    }

    return false;
}

bool Project::open()
{
    return GPFile::open();
}

bool Project::open(const QString &projectPath)
{
    qDebug() << "Opening project:" << projectPath;
    _path = projectPath;
    if(!open())
    {
        _error = tr("The project specified (%1) could not be opened"
                    ).arg(projectPath);
        return false;
    }

    // We need the file to exist to proceed, fail if the status is not Normal
    if(_status != GPFile::Normal)
    {
        _error = tr("The project specified (%1) did not exist"
                    ).arg(projectPath);
        return false;
    }

    // Parse the file, details of the format are in the documentation for the
    // Project class
    QString proj = _fp->readAll();
    if(proj.isEmpty())
    {
        _error = tr("The project specified (%1) was empty"
                    ).arg(projectPath);
        return false;
    }

    QDomDocument document("project");
    if(!document.setContent(proj))
    {
        _error = tr("The project file (%1) could not be parsed as XML"
                    ).arg(projectPath);
        return false;
    }

    // First get the top-level <project> element, fail if it cannot be found
    QDomNodeList nodes = document.elementsByTagName("project");
    if(nodes.isEmpty())
    {
        _error = tr("The project file (%1) provided could not be parsed as a GP"
                    "Developer project"
                    ).arg(projectPath);
        return false;
    }

    // Take the node returned, make it into an element, and then grab all of the
    // attributes associated with it
    QDomNode node = nodes.at(0);
    QDomElement projectElement = node.toElement();
    QString name = projectElement.attribute("name");
    QString gpVersion = projectElement.attribute(
                "gpversion",
                GPVersionToString(DEFAULT_GP_VERSION)
                );
    double gpDeveloperVersion = projectElement.attribute(
                "developerversion",
                QVariant(GP_DEVELOPER_VERSION).toString()
                ).toDouble();

    // A name must be present
    if(name.isEmpty())
    {
        _error = tr("The project file (%1) provided could not be parsed as a GP"
                    "Developer project"
                    ).arg(projectPath);
        return false;
    }

    setName(name);
    setGPVersion(stringToGPVersion(gpVersion));

    //! \todo When there are multiple versions of GP Developer, this is where
    //!     an update procedure would be triggered
    if(gpDeveloperVersion < GP_DEVELOPER_VERSION)
    {
        qDebug() << "Older version of GP Developer used to produce this project"
                 << ", that might be bad!";
    }

    //! \todo read in list of files (graphs, rules, programs, run configs)
    nodes = projectElement.childNodes();
    for(int i = 0; i < nodes.count(); ++i)
    {
        node = nodes.at(i);
        QDomElement elem = node.toElement();
        if(elem.tagName() == "rules")
        {
            if(!readRules(node))
            {
                _error = "Error while reading rule files";
                return false;
            }
        }
        else if(elem.tagName() == "programs")
        {
            if(!readPrograms(node))
            {
                _error = "Error while reading program files";
                return false;
            }
        }
        else if(elem.tagName() == "graphs")
        {
            if(!readGraphs(node))
            {
                _error = "Error while reading graph files";
                return false;
            }
        }
        else if(elem.tagName() == "runconfigurations")
        {
            if(!readRunConfigs(node))
            {
                _error = "Error while reading run configurations";
                return false;
            }
        }
    }

    emit fileListChanged();
    emit runConfigurationListChanged();

    _null = false;
    _error = "";
    _status = GPFile::Normal;
    qDebug() << "    Finished opening project.";
    emit statusChanged(_status);
    emit openComplete();
    return true;
}

bool Project::readRules(QDomNode &node)
{
    QDomNodeList nodes = node.childNodes();
    for(int i = 0; i < nodes.count(); ++i)
    {
        QDomNode n = nodes.at(i);
        QDomElement elem = n.toElement();

        if(elem.tagName() != "rule")
        {
            qDebug() << "Ignoring unexpected tag: " << elem.tagName();
            qDebug() << "GP Developer was expecting a: <rule>";
            continue;
        }

        QString path = elem.attribute("path");
        if(path.isEmpty())
        {
            qDebug() << "Read in <rule> tag with no path attribute, failing.";
            return false;
        }

        Rule *r = new Rule(path);
        connect(r, SIGNAL(statusChanged(FileStatus)),
                this, SLOT(trackRuleStatusChange(FileStatus))
                );
        _rules.push_back(r);
        emit ruleListChanged();
        emit fileListChanged();
    }

    return true;
}

bool Project::readPrograms(QDomNode &node)
{
    QDomNodeList nodes = node.childNodes();
    for(int i = 0; i < nodes.count(); ++i)
    {
        QDomNode n = nodes.at(i);
        QDomElement elem = n.toElement();

        if(elem.tagName() != "program")
        {
            qDebug() << "Ignoring unexpected tag: " << elem.tagName();
            qDebug() << "GP Developer was expecting a: <program>";
            continue;
        }

        QString path = elem.attribute("path");
        if(path.isEmpty())
        {
            qDebug() << "Read in <program> tag with no path attribute, failing.";
            return false;
        }

        Program *p = new Program(path);
        connect(p, SIGNAL(statusChanged(FileStatus)),
                this, SLOT(trackProgramStatusChange(FileStatus))
                );
        _programs.push_back(p);
        emit programListChanged();
        emit fileListChanged();
    }

    return true;
}

bool Project::readGraphs(QDomNode &node)
{
    QDomNodeList nodes = node.childNodes();
    for(int i = 0; i < nodes.count(); ++i)
    {
        QDomNode n = nodes.at(i);
        QDomElement elem = n.toElement();

        if(elem.tagName() != "graph")
        {
            qDebug() << "Ignoring unexpected tag: " << elem.tagName();
            qDebug() << "GP Developer was expecting a: <graph>";
            continue;
        }

        QString path = elem.attribute("path");
        if(path.isEmpty())
        {
            qDebug() << "Read in <graph> tag with no path attribute, failing.";
            return false;
        }

        Graph *g = new Graph(path, false);
        connect(g, SIGNAL(statusChanged(FileStatus)),
                this, SLOT(trackGraphStatusChange(FileStatus))
                );
        connect(g, SIGNAL(nodeAdded(Node*)), this, SLOT(incrementNodeCount()));
        connect(g, SIGNAL(edgeAdded(Edge*)), this, SLOT(incrementEdgeCount()));
        g->open();
        _graphs.push_back(g);
        emit graphListChanged();
        emit fileListChanged();
    }

    return true;
}

bool Project::readRunConfigs(QDomNode &node)
{
    QDomNodeList nodes = node.childNodes();
    for(int i = 0; i < nodes.count(); ++i)
    {
        QDomNode n = nodes.at(i);
        QDomElement elem = n.toElement();

        if(elem.tagName() != "runconfiguration")
        {
            qDebug() << "Ignoring unexpected tag: " << elem.tagName();
            qDebug() << "GP Developer was expecting a: <runconfiguration>";
            continue;
        }

        QString name = elem.attribute("name");
        // A name must be present
        if (name.isEmpty())
        {
            qDebug() << "Read in <runconfiguration> tag with no name attribute, failing.";
            return false;
        }

        QString program = elem.attribute("program");
        // A program must be present
        if (program.isEmpty())
        {
            qDebug() << "Read in <runconfiguration> tag with no program attribute, failing.";
            return false;
        }

        QString graph = elem.attribute("graph");
        // A graph must be present
        if (graph.isEmpty())
        {
            qDebug() << "Read in <runconfiguration> tag with no graph attribute, failing.";
            return false;
        }


        RunConfig *config = new RunConfig(this, name, program, graph);

        readRunConfigOptions(n, config);
        _runConfigurations.push_back(config);
        // emit runConfigurationListChanged();

    }

    return true;
}

void Project::readRunConfigOptions(QDomNode &node, RunConfig* config)
{
    QDomNodeList nodes = node.childNodes();
    for(int i = 0; i < nodes.count(); ++i)
    {
        QDomNode n = nodes.at(i);
        QDomElement elem = n.toElement();

        if(elem.tagName() != "options")
        {
            qDebug() << "Ignoring unexpected tag: " << elem.tagName();
            qDebug() << "GP Developer was expecting a: <options>";
            continue;
        }

				// Check for specific options - e.g. tracing and backtracking
				QDomNodeList options = n.childNodes();
				for (int j = 0; j < options.count(); ++j)
				{
				    n = options.at(j);
				    elem = n.toElement();

				    if(elem.tagName() == "tracing")
				    {
								bool trace = QVariant(elem.text()).toBool();
								config->setTracing(trace);
				        continue;
				    }
				    else if(elem.tagName() == "backtracking")
				    {
								bool backtrack = QVariant(elem.text()).toBool();
								config->setBacktracking(backtrack);
				        continue;
				    }	
						else 
						{
				        qDebug() << "Ignoring unexpected tag: " << elem.tagName();
				        qDebug() << "GP Developer was expecting a: <tracing> or <backtracking>";
								continue;
						}			
				}		
		}
}

void Project::incrementNodeCount()
{
    ++_nodeCount;
    emit nodeCountChanged(_nodeCount);
}

void Project::incrementEdgeCount()
{
    ++_edgeCount;
    emit edgeCountChanged(_edgeCount);
}

bool Project::initProject(const QString &targetPath, const QString &projectName, GPVersions gpVersion)
{
    QDir dir(targetPath);
    if(!dir.isAbsolutePath(targetPath))
        return false;

    if(!dir.exists())
    {
        QMessageBox::StandardButton reply;
        reply = QMessageBox::question(0, tr("Create Directory?"),
                                      tr("The directory specified (%1) does not"
                                         " exist, create it?").arg(targetPath),
                                      QMessageBox::Yes | QMessageBox::Cancel
                                      );
        if(reply != QMessageBox::Yes)
            return false;

        dir.mkpath(targetPath);
    }

    // Check for subdirectories called "rules", "programs", "graphs" or "results" - create
    // them automatically if they don't exist.
    QString path = targetPath;
    if(path.at(targetPath.length()-1) != dir.separator())
        path += QString(dir.separator());
    QString rulesPath = path + "rules";
    QString programsPath = path + "programs";
    QString graphsPath = path + "graphs";
    QString resultsPath = path + "results";

    QDir rules(rulesPath);
    if(!rules.exists())
        rules.mkpath(rulesPath);

    QDir programs(programsPath);
    if(!programs.exists())
        programs.mkpath(programsPath);

    QDir graphs(graphsPath);
    if(!graphs.exists())
        graphs.mkpath(graphsPath);

    QDir results(resultsPath);
    if(!results.exists())
        results.mkpath(resultsPath);

    QFile file(dir.filePath(projectName + GP_PROJECT_EXTENSION));
    _path = dir.filePath(projectName + GP_PROJECT_EXTENSION);
    file.open(QFile::WriteOnly);

    setName(projectName);

    /*
     * Write a basic template project file
     *
     * Replacements:
     *  %1 => Project name
     *  %2 => GP version
     *  %3 => GP Developer version
     */
    QFile fp(":/templates/newproject.gpp");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString newProject = fp.readAll();
    newProject = newProject.arg(
                projectName,
                GPVersionToString(gpVersion),
                QVariant(GP_DEVELOPER_VERSION).toString()
                );

    // First just open up the file, don't parse it
    if(!open())
        return false;

    _fp->write(QVariant(newProject).toByteArray());

    // Now we can parse it
    open(_path);

    return true;
}

void Project::newRule(const QString &ruleName)
{
    QString name = ruleName;
    QString filePath;
    QFileInfo info(name);
    // Have we been passed an absolute path?
    if(info.isAbsolute())
    {
        filePath = name;
        name = info.baseName();
        if(name.endsWith(GP_RULE_EXTENSION))
            name = name.left(name.size()-4);
    }
    else
    {
        // Get the directory in which we should be creating this rule
        QDir d = rulesDir();

        filePath = d.filePath(name + GP_RULE_EXTENSION);
    }

    QFile file(filePath);
    // Check if a rule with this name already exists
    if(file.exists())
    {
        QMessageBox::StandardButton response;
        response = QMessageBox::warning(
                    0,
                    tr("File Exists"),
                    tr("A file called \"%1\" already exists, do you want to "
                       "overwrite it?").arg(filePath),
                    QMessageBox::Yes | QMessageBox::Cancel
                    );
        // The user hit cancel, bail without creating a rule
        if(response != QMessageBox::Yes)
            return;
    }

    /*
     * Write a basic template rule file
     *
     * Replacements:
     *  %1 => Rule name
     *  %2 => Creation time
     */
    QFile fp(":/templates/newrule_alternative.gpr");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString newRuleString = fp.readAll();
    newRuleString = newRuleString.arg(
                name
                //,QDateTime::currentDateTime().toString("dd/MM/yyyy hh:mm:ss")
                );

    file.open(QFile::ReadWrite);
    file.write(QVariant(newRuleString).toByteArray());
    file.close();

    // Add the resulting file
    addRule(filePath);
}

void Project::newProgram(const QString &programName)
{
    QString name = programName;
    QString filePath;
    QFileInfo info(name);
    // Have we been passed an absolute path?
    if(info.isAbsolute())
    {
        filePath = name;
        name = info.baseName();
        if(name.endsWith(GP_PROGRAM_EXTENSION) || name.endsWith(GP_OLD_EXTENSION))
            name = name.left(name.size()-4);
    }
    else
    {
        // Get the directory in which we should be creating this program
        QDir d = programsDir();

        filePath = d.filePath(name + GP_PROGRAM_EXTENSION);
    }

    QFile file(filePath);
    // Check if a rule with this name already exists
    if(file.exists())
    {
        QMessageBox::StandardButton response;
        response = QMessageBox::warning(
                    0,
                    tr("File Exists"),
                    tr("A file called \"%1\" already exists, do you want to "
                       "overwrite it?").arg(filePath),
                    QMessageBox::Yes | QMessageBox::Cancel
                    );
        // The user hit cancel, bail without creating a program
        if(response != QMessageBox::Yes)
            return;
    }

    /*
     * Write a basic template rule file
     *
     * Replacements:
     *  %1 => Rule name
     *  %2 => Creation time
     */
    QFile fp(":/templates/newprogram.gp2");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString newProgramString = fp.readAll();
//    newProgramString = newProgramString.arg(
//                name,
//                QDateTime::currentDateTime().toString("dd/MM/yyyy hh:mm:ss")
//                );

    file.open(QFile::ReadWrite);
    file.write(QVariant(newProgramString).toByteArray());
    file.close();

    // Add the resulting file
    addProgram(filePath);
}

void Project::newGraph(const QString &graphName, GraphTypes type)
{
    QString name = graphName;

    // Determine the type the graph should be
    switch(type)
    {
//    case DotGraph:
//    case GxlGraph:
    case AlternativeGraph:
        // Already specified, move on
        break;
    case DefaultGraph:
    default:
        // Check if one is implied by the extension
        if(graphName.endsWith(GP_GRAPH_ALTERNATIVE_EXTENSION))
            type = AlternativeGraph;
//        else if(graphName.endsWith(GP_GRAPH_GXL_EXTENSION))
//            type = GxlGraph;
//        else if(graphName.endsWith(GP_GRAPH_DOT_EXTENSION))
//            type = DotGraph;
        else
            type = DEFAULT_GRAPH_FORMAT;
    }

    QString filePath;
    QFileInfo info(name);
    // Have we been passed an absolute path?
    if(info.isAbsolute())
    {
        filePath = name;
        name = info.baseName();
    }
    else
    {
        // Get the directory in which we should be creating this rule
        QDir d = graphsDir();

        // Work out the correct extension for this file
        if(graphName.endsWith(GP_GRAPH_DOT_EXTENSION)
                || graphName.endsWith(GP_GRAPH_GXL_EXTENSION)
                || graphName.endsWith(GP_GRAPH_ALTERNATIVE_EXTENSION))
        {
            filePath = d.filePath(graphName);
        }
        else
        {
            switch(type)
            {
//            case GxlGraph:
//                filePath = d.filePath(graphName + GP_GRAPH_GXL_EXTENSION);
//                break;
            case AlternativeGraph:
                filePath = d.filePath(graphName + GP_GRAPH_ALTERNATIVE_EXTENSION);
                break;
//            case DotGraph:
//                filePath = d.filePath(graphName + GP_GRAPH_DOT_EXTENSION);
//                break;
            default:
                filePath = d.filePath(graphName + GP_GRAPH_DEFAULT_EXTENSION);
                break;
            }
        }
    }

    QFile file(filePath);
    // Check if a rule with this name already exists
    if(file.exists())
    {
        QMessageBox::StandardButton response;
        response = QMessageBox::warning(
                    0,
                    tr("File Exists"),
                    tr("A file called \"%1\" already exists, do you want to "
                       "overwrite it?").arg(filePath),
                    QMessageBox::Yes | QMessageBox::Cancel
                    );
        // The user hit cancel, bail without creating a program
        if(response != QMessageBox::Yes)
            return;
    }

    file.open(QFile::ReadWrite);

    switch(type)
    {
//    case GxlGraph:
//        file.write(QVariant(QString("<graph></graph>")).toByteArray());
//        break;
    case AlternativeGraph:
    {
        QFile fp(":/templates/newgraph_alternative.host");
        fp.open(QIODevice::ReadOnly | QIODevice::Text);
        QString newGraphString = fp.readAll();
        file.write(QVariant(newGraphString).toByteArray());
    }
        break;
//    case DotGraph:
//        file.write(QVariant(QString("")).toByteArray());
//        break;
    default:
        file.write(QVariant(QString("[ | | ]")).toByteArray());
        break;
    }

    file.close();

    // Add the resulting file
    addGraph(filePath);
}

void Project::addRule(const QString &filePath)
{
    // Don't add non-existent files
    QFile f(filePath);
    if(!f.exists())
        return;

    // Don't re-add files which are already part of the project
    if(containsFile(filePath))
    {
        QMessageBox::warning(
                    0,
                    tr("File Already Present"),
                    tr("The file you have asked GP Developer to add to this "
                       "project (%1) is already being tracked. GP Developer "
                       "has not added the file again.").arg(filePath),
                    QMessageBox::Ok
                    );
        return;
    }

    Rule *rule = new Rule(filePath, this);
    connect(rule, SIGNAL(statusChanged(FileStatus)),
            this, SLOT(trackRuleStatusChange(FileStatus)));
    _rules.push_back(rule);
    save();
    emit ruleListChanged();
    emit fileListChanged();
}

void Project::addProgram(const QString &filePath)
{
    // Don't add non-existent files
    QFile f(filePath);
    if(!f.exists())
        return;

    // Don't re-add files which are already part of the project
    if(containsFile(filePath))
    {
        QMessageBox::warning(
                    0,
                    tr("File Already Present"),
                    tr("The file you have asked GP Developer to add to this "
                       "project (%1) is already being tracked. GP Developer "
                       "has not added the file again.").arg(filePath),
                    QMessageBox::Ok
                    );
        return;
    }

    Program *program = new Program(filePath, this);
    connect(program, SIGNAL(statusChanged(FileStatus)),
            this, SLOT(trackProgramStatusChange(FileStatus)));
    _programs.push_back(program);
    save();
    emit programListChanged();
    emit fileListChanged();
}

void Project::addGraph(const QString &filePath)
{
    // Don't add non-existent files
    QFile f(filePath);
    if(!f.exists())
        return;

    // Don't re-add files which are already part of the project
    if(containsFile(filePath))
    {
        QMessageBox::warning(
                    0,
                    tr("File Already Present"),
                    tr("The file you have asked GP Developer to add to this "
                       "project (%1) is already being tracked. GP Developer "
                       "has not added the file again.").arg(filePath),
                    QMessageBox::Ok
                    );
        return;
    }

    Graph *graph = new Graph(filePath, true, this);
    connect(graph, SIGNAL(statusChanged(FileStatus)),
            this, SLOT(trackGraphStatusChange(FileStatus)));
    _graphs.push_back(graph);
    save();
    emit graphListChanged();
    emit fileListChanged();
}


void Project::removeConfig(RunConfig* runConfig)
{
    if (!containsRunConfig(runConfig))
    {
        qDebug() << "Attempted to remove an unknown Run Config, ignoring." << runConfig->name();
        return;
    }

    for (runConfigIter it = _runConfigurations.begin(); it != _runConfigurations.end(); )
    {
        RunConfig* config = *it;
        if (config == runConfig)
        {
            it = _runConfigurations.erase(it);
            save();
            emit runConfigurationListChanged();
            break;
        }
        else
            ++it;
    }

}

bool Project::addRunConfig(RunConfig *runConfig)
{
    if(containsRunConfigName(runConfig->name()))
    {
        qDebug() << "Attempted to add an already known Run Configuration; ignoring" << runConfig->name();
        return false;
    }

    _runConfigurations.push_back(runConfig);
    save();
    emit runConfigurationListChanged();
    return true;
}

void Project::setCurrentFile(const QString &fileName, FileTypes type)
{
    // If the file isn't here, it can't be the current file
    if(!containsFile(fileName))
        return;

    GPFile *f = 0;
    switch(type)
    {
    case RuleFile:
        f = rule(fileName);
        break;
    case GraphFile:
        f = graph(fileName);
        break;
    case ProgramFile:
        f = program(fileName);
        break;
    default:
        f = 0;
        qDebug() << "Invalid FileType passed into Project::setCurrentFile()";
    }

    if(f == 0)
        return;

    _currentFile = f;
    emit currentFileChanged(f);
}

bool Project::containsFile(const QString &filePath)
{
    return (containsGraph(filePath) || containsProgram(filePath)
                || containsRule(filePath));
}

bool Project::containsGraph(const QString &filePath)
{
    return (graph(filePath) != 0);
}

bool Project::containsRule(const QString &filePath)
{
    return (rule(filePath) != 0);
}

bool Project::containsProgram(const QString &filePath)
{
    return (program(filePath) != 0);
}

bool Project::containsRunConfig(RunConfig* runConfig)
{
    for(runConfigConstIter iter = _runConfigurations.begin(); iter != _runConfigurations.end(); ++iter)
    {
        RunConfig *config = *iter;
        if (config == runConfig)    // Compare objects themselves
        {
            qDebug() << "  project.cpp: Found Run Configuration: " << runConfig->name();
            return true;
        }
    }

    return false;
}

bool Project::containsRunConfigName(QString runConfigName)
{
    for(runConfigConstIter iter = _runConfigurations.begin(); iter != _runConfigurations.end(); ++iter)
    {
        RunConfig *config = *iter;
        if (config->name() == runConfigName)    // Compare object names
        {
            qDebug() << "  project.cpp: Found Run Configuration: " << runConfigName;
            return true;
        }
    }

    return false;
}

RunConfig* Project::runConfig(QString &configName)
{
    for(runConfigConstIter iter = _runConfigurations.begin(); iter != _runConfigurations.end(); ++iter)
    {
        RunConfig *config = *iter;
        if (config->name() == configName)    // Compare objects themselves
        {
            qDebug() << "  project.cpp: Found Run Configuration: " << configName;
            return config;
        }
    }

    return 0;
}

bool Project::save()
{
    QDomDocument doc("project");

    // We require that this file already exists for this type of save operation
    if(!_fp->exists())
        return false;

    QDomElement root =  doc.createElement("project");
    root.setAttribute("name", name());
    root.setAttribute("gpVersion", GPVersionToString(gpVersion()));
    root.setAttribute("gpDeveloperVersion", GP_DEVELOPER_VERSION);
    doc.appendChild(root);

    QDomElement rules = doc.createElement("rules");
    root.appendChild(rules);

    for(ruleIter iter = _rules.begin(); iter != _rules.end(); ++iter)
    {
        Rule *rule = *iter;
        QDomElement ruleTag = doc.createElement("rule");
        //ruleTag.setAttribute("name", rule->name());
        ruleTag.setAttribute("path", rule->path());
        rules.appendChild(ruleTag);
    }

    QDomElement programs = doc.createElement("programs");
    root.appendChild(programs);

    for(programIter iter = _programs.begin(); iter != _programs.end(); ++iter)
    {
        Program *program = *iter;
        QDomElement programTag = doc.createElement("program");
        //programTag.setAttribute("name", program->name());
        programTag.setAttribute("path", program->path());
        programs.appendChild(programTag);
    }

    QDomElement graphs = doc.createElement("graphs");
    root.appendChild(graphs);

    for(graphIter iter = _graphs.begin(); iter != _graphs.end(); ++iter)
    {
        Graph *graph = *iter;
        QDomElement graphTag = doc.createElement("graph");
        graphTag.setAttribute("path", graph->path());
        graphs.appendChild(graphTag);
    }

    //! \todo Save run configurations
    QDomElement runConfigurations = doc.createElement("runconfigurations");
    root.appendChild(runConfigurations);

    for(runConfigConstIter iter = _runConfigurations.begin(); iter != _runConfigurations.end(); ++iter)
    {
        RunConfig *config = *iter;
        if (!config)
            continue;
        if (config->name() == QString() || config->name() == QString(""))
            continue;
        // qDebug() << "    project.cpp: Found valid run config" << config->name();
        QDomElement configTag = doc.createElement("runconfiguration");
        configTag.setAttribute("name", config->name());
        configTag.setAttribute("program", config->program());
        configTag.setAttribute("graph", config->graph());

        /*
        QDomElement configOptions = doc.createElement("options");
        if (config->hasTracing())
        {
            QDomElement tracing = doc.createElement("tracing");
            QDomText traceText = doc.createTextNode(QString(config->getTracing()));
            tracing.appendChild(traceText);

            configOptions.appendChild(tracing);
        }
        if (config->hasBacktracking())
        {
            QDomElement backTracking = doc.createElement("backtracking");
            QDomText backTrackingText = doc.createTextNode(QString(config->getBacktracking()));
            backTracking.appendChild(backTrackingText);

            configOptions.appendChild(backTracking);
        }*/

        runConfigurations.appendChild(configTag);
    }

    _fp->close();
    _fp->open(QFile::Truncate | QFile::WriteOnly);

    _fp->write(doc.toByteArray());

    _fp->close();
    _fp->open(QFile::ReadWrite);

    return true;
}

bool Project::saveAs(const QString &filePath)
{
    // Can't save to nowhere
    if(filePath.isEmpty())
        return false;

    // Update the filesystem watcher
    if(!GPFile::saveAs(filePath))
        return false;

    QFileInfo info(filePath);
    if(!info.dir().exists())
    {
        QMessageBox::StandardButton response;
        response = QMessageBox::warning(
                    0,
                    tr("Directory Not Found"),
                    tr("The directory specified (%1) does not exist. Create it?"
                       ).arg(info.dir().path()),
                    QMessageBox::Yes | QMessageBox::Cancel
                    );
        // The user hit cancel, bail without changing anything
        if(response != QMessageBox::Yes)
            return false;
    }

    //! \todo Decide whether this should convert all paths to absolute ones
    //!     based on the old project location, if it should transfer across all
    //!     of the relative files which are within the project directory, or if
    //!     it should offer a combination of these as a user choice

    _path = filePath;
    return save();
}

bool Project::saveFile(QString filePath)
{
    GPFile *f;
    if(filePath.isEmpty())
        f = _currentFile;
    else
        f = file(filePath);

    if(f == 0)
        return false;

    return f->save();
}

bool Project::saveCurrentFile()
{
    return saveFile();
}

bool Project::saveFileAs(const QString &filePath)
{
    GPFile *f;
    if(filePath.isEmpty())
        f = _currentFile;
    else
        f = file(filePath);

    if(f == 0)
        return false;

    // Commented because it is probably better to handle this in the derived
    // classes since they can use more specific filters.
    /*QString savePath = newPath;
    if(savePath.isEmpty())
    {
        savePath = QFileDialog::getSaveFileName(
                    0,
                    tr("Save File As"),
                    dir().path(),
                    tr("GP Files (*.gpr *.gpx *.gv *.gxl)")
                    );

        // If there is still not a path then the user has canceled
        if(savePath.isEmpty())
            return false;
    }*/

    if(f->saveAs())
    {
        emit fileListChanged();
        // The save operation succeeded, therefore we need to save the project
        // in order to get the path correct
        return save();
    }
    else
        return false;
}

bool Project::saveCurrentFileAs()
{
    return saveFileAs();
}

bool Project::saveAll()
{
    return false;
}

void Project::exec()
{
    open(_path);
}

void Project::fileModified(QString filePath)
{
    // This shouldn't happen, but just in case
    if(!containsFile(filePath))
        return;

    // We need to signal that a change has been made to interested classes
    if(containsGraph(filePath))
        emit graphChanged(filePath);
    else if(containsRule(filePath))
        emit ruleChanged(filePath);
    else if(containsProgram(filePath))
        emit programChanged(filePath);
    emit fileChanged(filePath);
}

void Project::trackRuleStatusChange(FileStatus status)
{
    Q_UNUSED(status)
    Rule *rule = static_cast<Rule *>(sender());
    emit ruleStatusChanged(rule->path(), rule->status());
    emit fileStatusChanged(rule->absolutePath(), rule->status());
}

void Project::trackProgramStatusChange(FileStatus status)
{
    Q_UNUSED(status)
    Program *program = static_cast<Program *>(sender());
    emit programStatusChanged(program->path(), program->status());
    emit fileStatusChanged(program->absolutePath(), program->status());
}

void Project::trackGraphStatusChange(FileStatus status)
{
    Q_UNUSED(status)
    Graph *graph = static_cast<Graph *>(sender());
    emit graphStatusChanged(graph->path(), graph->status());
    emit fileStatusChanged(graph->absolutePath(), graph->status());
}

}
