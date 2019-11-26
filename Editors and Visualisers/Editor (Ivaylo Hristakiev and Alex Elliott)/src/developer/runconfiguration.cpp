/*!
 * \file
 */
#include "runconfiguration.hpp"
#include "ui_runconfiguration.h"

#include "project.hpp"
#include "rule.hpp"

#include <QFile>
#include <QByteArray>
#include <QFileInfo>
#include <QFile>
#include <QMessageBox>
#include <QProcess>

#include <stdlib.h>  /* system, NULL, EXIT_FAILURE */

#ifndef COMPILER_LOCATION
  #define COMPILER_LOCATION @COMPILER_LOCATION@
#endif

namespace Developer {

class Rule;

RunConfiguration::RunConfiguration(Project *proj, QWidget *parent, RunConfig* runConfig)
    : QWidget(parent)
    , _ui(new Ui::RunConfiguration)
    , _project(proj)
    , _runs(0)
    , _config(runConfig)
{
    _ui->setupUi(this);

    // Load the main stylesheet and apply it to this widget
    QFile fp(":/stylesheets/runconfiguration.css");
    fp.open(QIODevice::ReadOnly | QIODevice::Text);
    QString style = fp.readAll();
    setStyleSheet(style);

    _ui->detailsWidget->setVisible(false);

    toggleDetails();
    updatePrograms();
    updateGraphs();

    _ui->configurationNameEdit->setValidator(new QRegExpValidator(QRegExp("^(\\w|\\d|\\-|_|\\.)+$"),this));
    if (_config)
    {
        _ui->configurationNameEdit->setText(_config->name());

        int index = _ui->programCombo->findText(_config->program());
        if (index != -1)
            _ui->programCombo->setCurrentIndex(index);

        index = _ui->targetGraphCombo->findText(_config->graph());
        if (index != -1)
            _ui->targetGraphCombo->setCurrentIndex(index);
    }
}

RunConfiguration::~RunConfiguration()
{
    delete _ui;
    if (_config)
        delete _config;
}

RunConfig *RunConfiguration::getRunConfig()
{
    return _config;
}

QString RunConfiguration::name() const
{
    return _ui->configurationNameEdit->text();
}

QString RunConfiguration::program() const
{
    return _ui->programCombo->currentText();
}

QString RunConfiguration::graph() const
{
    return _ui->targetGraphCombo->currentText();
}


void RunConfiguration::setName(QString name)
{
    _ui->configurationNameEdit->setText(name);
    if (_config)
        _config->setName(name);
}

void RunConfiguration::setProgram(QString programName)
{
    updatePrograms();

    int index = _ui->programCombo->findText(programName);
    if (index != -1)
        _ui->programCombo->setCurrentIndex(index);

    if (_config)
        _config->setProgram(programName);
}

void RunConfiguration::setGraph(QString graphName)
{
    updateGraphs();

    int index = _ui->targetGraphCombo->findText(graphName);
    if (index != -1)
        _ui->targetGraphCombo->setCurrentIndex(index);

    if (_config)
        _config->setGraph(graphName);
}




void RunConfiguration::toggleDetails()
{
    _ui->detailsWidget->setVisible(!_ui->detailsWidget->isVisible());

    if(_ui->detailsWidget->isVisible())
        _ui->arrowImage->setPixmap(QPixmap(":/icons/bullet_arrow_down.png"));
    else
        _ui->arrowImage->setPixmap(QPixmap(":/icons/bullet_arrow_right.png"));
}

void RunConfiguration::updatePrograms()
{
    _ui->programCombo->clear();
    QVector<Program *> programs = _project->programs();
    for(QVector<Program *>::iterator iter = programs.begin();
        iter != programs.end(); ++iter)
    {
        Program *prog = *iter;
        if(prog == 0)
        {
            qDebug() << "Null pointer in RunConfiguration::updatePrograms(), "
                        "ignoring.";
        }
        _ui->programCombo->addItem(prog->name());
    }
}

void RunConfiguration::updateGraphs()
{
    _ui->targetGraphCombo->clear();
    QVector<Graph *> graphs = _project->graphs();
    for(QVector<Graph *>::iterator iter = graphs.begin();
        iter != graphs.end(); ++iter)
    {
        Graph *graph = *iter;
        if(graph == 0)
        {
            qDebug() << "Null pointer in RunConfiguration::updateGraphs(), "
                        "ignoring.";
        }
        _ui->targetGraphCombo->addItem(graph->fileName());
    }
}

void RunConfiguration::runConfiguration()
{

    QString configName =  _ui->configurationNameEdit->text();
    qDebug() << "Running configuration" << configName;

    QString progName = _ui->programCombo->currentText();
    Program* prog = _project->program(progName);    // exploiting that project->program(path) first iterates through _programs and compares program.name() with path
    if (!prog || !prog->save())
    {
        // Couldn't save the specified program
        QMessageBox::warning(
                    this,
                    tr("Saving Program Failed"),
                    tr("Could not save the program %1 before running. Check the log for details.")
                    .arg(prog->baseName())
                    );
        return;
    }


    QString programString = prog->program();
    //program = qPrintable(prog->absolutePath());

    // Collect all rule specifications, we have to append them to the program text
    QVector<Rule *> rules =  _project->rules();

    QStringList resultList;
    for (QVector<Rule *>::iterator it = rules.begin(); it!= rules.end(); ++it)
    {
        Rule* rule = *it;

        // First save the rule
        if (!rule || !rule->save())
        {
            // Couldn't save on of the rules
            QMessageBox::warning(
                        this,
                        tr("Saving Program Failed"),
                        tr("Could not save the rule %1 before running. Check the log for details.")
                        .arg(rule->name())
                        );
            return;
        }
        else
        {
            resultList << rule->toAlternative();
        }
    }

    QString ruleStrings = resultList.join("\n");

    // Create temporary file to hold program text and rules
    QString programTmp = prog->absolutePath() + ".tmp";
    QFile file(programTmp);  
  
    if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate | QIODevice::Text)) 
    {
        qDebug() << "    Could not open a temporary file for writing: " << programTmp;
    }
    else 
    {
        QTextStream content(&file);
        content << programString << "\n\n" << ruleStrings;
    }
    file.close();

    /* Locate the selected GP host graph */

    QString graphName = _ui->targetGraphCombo->currentText();
    Graph* graph = _project->graph(graphName);
    if (!graph || !graph->save())
    {
        // Couldn't save the specified graph
        QMessageBox::warning(
                    this,
                    tr("Saving Host Graph Failed"),
                    tr("Could not save the graph %1 before running. Check the log for details.")
                    .arg(graphName)
                    );
        return;
    }

    QString hostgraphFile = graph->absolutePath();


    //hostgraph = "~/github/GP2Test/hostgraphs/1.graph";

    // Update the corresponding RunConfig object
    // _config = _project->runConfig(configName);

    if (!_config)
        _config = new RunConfig(_project, configName, prog->name(), graph->fileName());

    else
    {
        _config->setName(configName);
        _config->setProgram(prog->name());
        _config->setGraph(graph->fileName());
    }

    // Update the project
    // Check the current configuration is non-local (i.e. it exists in the project already)
    _existsInProject = _project->containsRunConfig(_config);

    if (!_existsInProject)
    {
        // If local, then save it to project
        bool _addSuccess =  _project->addRunConfig(_config);

        if (!_addSuccess)
        {
            // Local and couldn't save to project, probably name clash
            QMessageBox::information(
                        this,
                        tr("Saving Run Configuration Failed"),
                        tr("There is already an existing run configuration with the same name."));
            return;
        }
    }

    // This will save the project file (.gpp) along with the run congfigurations
    // Will not do a recursive save on all files
    _project->save();

    /* Desired location of output */
    // The actual output is put by default in /tmp/gp2/gp2.output
    QDir resultsDir = _project->resultsDir();
    QString results = resultsDir.absolutePath();

    //qDebug() << "Results dir is: " << results << ", exists: " <<  resultsDir.exists() << ", isReadable: " << resultsDir.isReadable();

    QString output = results + "/Graph"+ QVariant(_runs).toString() + "_" + configName + ".host"; //eg. project/results/RunConfig1_run1.gpg  or project/results/RunConfig1_run2.gpg etc.
    QFileInfo checkFile(output);
    if (checkFile.exists())
    {
        qDebug() << "    Truncating the file: " << output;
        QFile outputFile(output);
        outputFile.open(QFile::WriteOnly|QFile::Truncate);
        outputFile.close();
    }

    /* Call the compiler and run the executable */
    bool success = run(programTmp, hostgraphFile, output);
    if (!success)
    {
        // The Compiler failed to validate/compile/execute (instead of giving a proper Fail)
        return;
    }
    _runs ++;

    /*
      Check for failure - represented as a string in the output
      The compiler uses the following code to represent failure:
        fprintf(output_file, \"No output graph: rule %s not applicable.\\n\")
        fprintf(output_file, \"No output graph: Fail statement invoked\\n\")
    */
    bool failure = false;

    QFile result(output);
    // Attempt to open the output file for reading
    if (!result.open(QIODevice::ReadOnly | QIODevice::Text))
    {
        qDebug() << "  " << "    Could not open result file for reading: " << output;
        return;
    }
    else
    {
        // Create input text stream for reading the file contents
        QTextStream data(&result);
        if (!data.atEnd())
        {
            // Read first line of output
            QString line = data.readLine();

            // Compare with failure string
            failure = line.startsWith("No output graph");
            if (failure)
            {
                qDebug() << "  Failure detected: " << output;
            }
        }
    }

    result.close();

    if (failure)
    {
        QMessageBox::information(
                    this,
                    tr("Run Failed"),
                    tr("Running the program produced Failure"));
        return;
    }

    qDebug() << "    Attempting to open output graph.";

    //Graph* resultGraph = new Graph(output, this);
    emit obtainedResultGraph(output, _config);
}

bool RunConfiguration::run(QString programFile, QString graphFile, QString outputFile)
{
    /* Location of GP Compiler */
    //QString GPCompilerDir = "~/github/GP2/Compiler";
    QString Compiler = QString(COMPILER_LOCATION);
    //QString Compiler = "gp2compile";
    QStringList args = QStringList();

    //qDebug() << "Location of GP2 Compiler: " << Compiler;

    QProcess validate;
    // http://stackoverflow.com/questions/3852587/how-to-get-stdout-from-a-qprocess
    validate.setProcessChannelMode(QProcess::MergedChannels);

    /* ****************** */
    /* Validate each rule */
    /* ****************** */

    // Collect all rules
    QVector<Rule *> rules =  _project->rules();
    qDebug () << "  Attempting to validate Rules.";

    for (QVector<Rule *>::iterator it = rules.begin(); it!= rules.end(); ++it)
    {
        Rule* rule = *it;
        args.clear();
        args << "-r" << rule->absolutePath().replace(" ","\\ ");

        validate.start(Compiler, args);

        if (!validate.waitForStarted())
        {
            QMessageBox::information(
                        this,
                        tr("Validation Failed"),
                        tr("Could not start validating one of the rules."));
            validate.close();
            return false;
        }

        if (!validate.waitForFinished()) // sets current thread to sleep
        {
            QMessageBox::information(
                        this,
                        tr("Validation Failed"),
                        tr("Could not finish validating one of the rules."));
            validate.close();
            return false;
        }

        QByteArray validateOutput = validate.readAllStandardOutput();
        qDebug() << "    " << QString(validateOutput).simplified();
        if (!validateOutput.contains("is valid"))
        {
            QMessageBox::information(
                        this,
                        tr("Validation Failed"),
                        tr("Could not validate one of the rules. See the log for details."));
            validate.close();
            return false;
        }
        validate.close();
    }
    qDebug () << "  Validation of Rules completed.";


    /* ******************************* */
    /* Validate program and host graph */
    /* ******************************* */
    args.clear();
    args << "-p" << programFile.replace(" ","\\ ");

    validate.start(Compiler, args);
    qDebug () << "  Attempting to validate Program:" << Compiler << args;

    if (!validate.waitForStarted())
    {
        QMessageBox::information(
                    this,
                    tr("Validation Failed"),
                    tr("Could not start validating the given program."));
        validate.close();
        return false;
    }

    if (!validate.waitForFinished()) // sets current thread to sleep
    {
        QMessageBox::information(
                    this,
                    tr("Validation Failed"),
                    tr("Could not finish validating the given program."));
        validate.close();
        return false;
    }

    QByteArray validateOutput = validate.readAllStandardOutput();
    qDebug() << "    " << QString(validateOutput).simplified();
    if (!validateOutput.contains("is valid"))
    {
        QMessageBox::information(
                    this,
                    tr("Validation Failed"),
                    tr("Could not validate the given configartion. See the log for details."));
        validate.close();
        return false;
    }
    validate.close();

    // do the same for the host graph
    args.clear(); args << "-h" << graphFile.replace(" ","\\ ");
    validate.start(Compiler, args);
    qDebug () << "  Attempting to validate Host Graph:" << Compiler << args;

    if (!validate.waitForStarted())
    {
        QMessageBox::information(
                    this,
                    tr("Validation Failed"),
                    tr("Could not start validating the given host graph."));
        validate.close();
        return false;
    }

    if (!validate.waitForFinished())
    {
        QMessageBox::information(
                    this,
                    tr("Validation Failed"),
                    tr("Could not finish validating the given host graph."));
        validate.close();
        return false;
    }

    validateOutput = validate.readAllStandardOutput();
    qDebug() << "    " << QString(validateOutput).simplified();
    if (!validateOutput.contains("is valid"))
    {
        QMessageBox::information(
                    this,
                    tr("Validation Failed"),
                    tr("Could not validate the given host graph. See the log for details."));
        validate.close();
        return false;
    }
    validate.close();

    /* *************************************************** */
    /* Create command for compiling program and host graph */
    /* *************************************************** */
    args.clear();
    args << programFile.replace(" ","\\ ") << graphFile.replace(" ","\\ ");

    // Clear the output directory first;
    QString tempPath = "/tmp/gp2";
    QFileInfo tmp(tempPath);
    if (tmp.exists() && tmp.isDir())
    {
        qDebug () << "  Cleaning temporary directory" << tempPath;
        QDir dir(tempPath);
        dir.setNameFilters(QStringList() << "*.*");
        dir.setFilter(QDir::Files);
        foreach(QString dirFile, dir.entryList())
        {
            dir.remove(dirFile);
        }
    }

    // The actual output is put by default in /tmp/gp2/gp2.output
    QProcess compile;
    // http://stackoverflow.com/questions/3852587/how-to-get-stdout-from-a-qprocess
    compile.setProcessChannelMode(QProcess::MergedChannels);

    qDebug () << "  Attempting to Compile configuration:" << Compiler << args;
    compile.start(Compiler, args);
    if (!compile.waitForStarted())
    {
        QMessageBox::information(
                    this,
                    tr("Compilation Failed"),
                    tr("Could not start compiling the given configartion."));
        compile.close();
        return false;
    }

    if (!compile.waitForFinished())
    {
        QMessageBox::information(
                    this,
                    tr("Compilation Failed"),
                    tr("Could not finish compiling the given configartion."));
        compile.close();
        return false;
    }

    QByteArray compileOutput = compile.readAllStandardOutput();
    qDebug() << "    " << QString(compileOutput).simplified();
    if (!compileOutput.contains("Generating program code...") || compileOutput.contains("Segmentation"))
    {
        QMessageBox::information(
                    this,
                    tr("Compilation Failed"),
                    tr("Could not compile the given configartion. See the log for details."));
        compile.close();
        return false;
    }
    compile.close();

    /* *********************************************************** */
    /* Create command for running the GP program on the host graph */
    /* *********************************************************** */
    QString RunCmd = QString();
    RunCmd += "cd /tmp/gp2";
    RunCmd += " && make && ";
    RunCmd += "./gp2run && cp gp2.output " + outputFile.replace(" ","\\ ");

    qDebug () << "  Attempting to execute GP2 Program:" << RunCmd;
//    bool success = (call(RunCmd) == 0);
    bool success = true;
    QProcess run;
    // http://stackoverflow.com/questions/3852587/how-to-get-stdout-from-a-qprocess
    run.setProcessChannelMode(QProcess::MergedChannels);

    args.clear();
    args << "-c" <<RunCmd;
    run.start("sh", args);
    if (!run.waitForStarted())
    {
        QMessageBox::information(
                    this,
                    tr("Run Failed"),
                    tr("Could not start running the given configartion."));
        run.close();
        qDebug () << "    Run failed.";
        return false;
    }

    if (!run.waitForFinished())
    {
        QMessageBox::information(
                    this,
                    tr("Run Failed"),
                    tr("Could not finish running the given configartion."));
        run.close();
        qDebug () << "    Run failed.";
        return false;
    }

    QByteArray runOutput = run.readAllStandardOutput();
    qDebug() << "    " << runOutput;
    if (!success || runOutput.contains("Error"))
    {
        QMessageBox::information(
                    this,
                    tr("Run Failed"),
                    tr("There was something wrong with execution. See the log for details."));
        run.close();
        qDebug () << "    Run failed.";
        return false;
    }
    qDebug () << "    Run successful.";
    run.close();

    return true;
}

// Return 1 -- error during execution
// Return 0 -- all fine
int RunConfiguration::call(QString cmd)
{
    // The mechanism for calling the GP compiler must be available
    //qDebug() << "Checking if command processor is available...";
    if (!system(NULL)) //qDebug() << "Ok";
    {
        qDebug() << "Command processor is not available;"
                      "stopping." ;
        return 1;
    }

    // printf ("Executing command...\n");

    // file stream
    FILE *fp; 
	
    /* Open the command for reading */
    QByteArray ba = cmd.toLatin1();
    const char* command = ba.data();

    qDebug() << "   Command is: ";
    qDebug() << "  " << QString(command);

    fp = popen(command, "r");
    char path[1035]; // output line

    if (fp == NULL)
    {
        qDebug() << "  " << "Failed to run %s." << cmd;
        return 1;
    }

    /* Read the output a line at a time and output it */
    qDebug() << "  " << "=== GP2 Pipeline output ===";
    while (fgets(path, sizeof(path)-1, fp) != NULL)
        qDebug() << "  " << path;

    /* close stream */
    qDebug() << "  " << "===========================";
    qDebug() << "";
    pclose(fp);
    return 0;
}


}

void Developer::RunConfiguration::on_deleteButton_clicked()
{
    QMessageBox::StandardButton response;
    response = QMessageBox::warning(
                0,
                tr("Deleting Run Configuration"),
                tr("Are you sure you want to delete this Run Configuration?"),
                QMessageBox::Yes | QMessageBox::Cancel
                );
    // The user hit cancel, bail without changing anything
    if(response != QMessageBox::Yes)
        return;

    if (_existsInProject && _project && _config)
    {
        _project->removeConfig(_config);
        qDebug() << "  runconfiguration.cpp: Removing" << _config->name() << "from project.";
    }
    delete this;
}
