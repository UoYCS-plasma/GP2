/*!
 * \file
 */
#include "rule.hpp"
#include "ruleparser.hpp"

#include "graph.hpp"

#include <QFileDialog>
#include <QDebug>
#include <QRegExp>

#include <algorithm>

namespace Developer {

Rule::Rule(const QString &rulePath, QObject *parent)
    : GPFile(rulePath, parent)
    , _name("")
    , _documentation("")
    , _lhs(0)
    , _rhs(0)
    , _condition("")
    , _options(Rule_DefaultBehaviour)
{
    _initialOpen = true;
    if(!rulePath.isEmpty())
        open();
    _initialOpen = false;
}

const QString &Rule::name() const
{
    return _name;
}

const QString &Rule::documentation() const
{
    return _documentation;
}

Graph *Rule::lhs() const
{
    return _lhs;
}

Graph *Rule::rhs() const
{
    return _rhs;
}

interface_t Rule::interface() const
{
    return _interface;
}

std::vector<param_t> Rule::variables() const
{
    return _variables;
}

const QString &Rule::condition() const
{
    return _condition;
}

int Rule::options() const
{
    return _options;
}

bool Rule::injectiveMatching() const
{
    return (_options & Rule_InjectiveMatching);
}

void Rule::setName(const QString &ruleName)
{
    if(ruleName.isEmpty() || ruleName == _name)
        return;

    _name = ruleName;
    _status = Modified;
    emit statusChanged(_status);
}

void Rule::setDocumentation(const QString &docString)
{
    if(docString.isEmpty() || docString == _documentation)
        return;

    _documentation = docString;

    if (_initialOpen)
        return;

    _status = Modified;
    emit statusChanged(_status);
}

void Rule::setLhs(Graph *lhsGraph)
{
    if (_rhs != 0)
        disconnect(_lhs, SIGNAL(statusChanged(FileStatus)), this, SLOT(lhsGraphChanged()));

    _lhs = lhsGraph;
    connect(_lhs, SIGNAL(statusChanged(FileStatus)), this, SLOT(lhsGraphChanged()));

    if (_initialOpen)
        return;

    _status = Modified;
    emit statusChanged(_status);
}

void Rule::setRhs(Graph *rhsGraph)
{
    if (_rhs != 0)
        disconnect(_rhs, SIGNAL(statusChanged(FileStatus)), this, SLOT(rhsGraphChanged()));

    _rhs = rhsGraph;
    connect(_rhs, SIGNAL(statusChanged(FileStatus)), this, SLOT(rhsGraphChanged()));

    if (_initialOpen)
        return;

    _status = Modified;
    emit statusChanged(_status);
}


void Rule::setInterface(interface_t &interface)
{
    _interface = interface;
		// Graph is an Qobject, but interface_t is not; TODO: rethink how to fix this
    //connect(_interface, SIGNAL(statusChanged(FileStatus)), this, SLOT(interfaceChanged()));

    std::vector<std::string> elements = _interface.elements;

    std::vector<Node*> lhsNodes = _lhs->nodes();
    for (std::vector<Node*>::const_iterator it = lhsNodes.begin(); it!= lhsNodes.end() ; ++it )
    {
        Node* lhsNode = *it;
        if (lhsNode)
            if (std::find(elements.begin(), elements.end(), lhsNode->id().toStdString()) != elements.end() )
                lhsNode->setIsInterface(true);
            else
                lhsNode->setIsInterface(false);
    }


    std::vector<Node*> rhsNodes = _rhs->nodes();
    for (std::vector<Node*>::const_iterator it = rhsNodes.begin(); it!= rhsNodes.end() ; ++it )
    {
        Node* rhsNode = *it;
        if (rhsNode)
            if (std::find(elements.begin(), elements.end(), rhsNode->id().toStdString()) != elements.end() )
                rhsNode->setIsInterface(true);
            else
                rhsNode->setIsInterface(false);
    }

//    for(std::vector<std::string>::iterator it = elements.begin(); it != elements.end() ; ++it)
//    {
//        QString var = QString::fromStdString(*it);
//        if(_lhs->containsNode(var))
//        {
//            Node* node = _lhs->node(var);
//            node->setIsInterface(true);
//        }
//        else
//            qDebug() << "Warning: Node" << var << "is not contained in LHS of rule"  << _name <<", but it is in the interface";

//        if(_rhs->containsNode(var))
//        {
//            Node* node = _rhs->node(var);
//            node->setIsInterface(true);
//        }
//        else
//            qDebug() << "Warning: Node" << var << "is not contained in RHS of rule"  << _name <<", but it is in the interface";
//    }


    if (_initialOpen)
        return;

    _status = Modified;
    emit statusChanged(_status);
}

void Rule::setVariables(std::vector<param_t> &variables)
{
    _variables = variables;
    // Graph is an Qobject, but std::vector<param_t> is not; TODO: rethink how to fix this
    //connect(_variables, SIGNAL(statusChanged(FileStatus)), this, SLOT(variablesChanged()));
    // qDebug() << "rule.cpp: Variables modified";

    // Right now variables are inferred, no need to emit a modify
//    if (_initialOpen) return;
//    _status = Modified;
//    emit statusChanged(_status);
}

void Rule::modifyVariables()
{
    if (_initialOpen) return;

    _status = Modified;
    emit statusChanged(_status);
}

void Rule::setCondition(const QString &conditionString)
{
    //qDebug() << _condition;
    if(_condition.compare(conditionString) == 0)
        return;

    _condition = conditionString;

    if (_initialOpen) return;
    _status = Modified;
    emit statusChanged(_status);
}

void Rule::setOptions(int options)
{
    // Check if this is valid input, remove all known option settings and if any
    // bits are still set then this is invalid and we should warn the user.
    int copy = options;
    copy &= ~Rule_InjectiveMatching;

    if(copy)
    {
        qDebug() << "Value passed to Rule::setOptions was not a valid "
                    "combination of settings.";
        return;
    }
    else
        _options = options;
}

void Rule::setInjectiveMatching(bool injective)
{
    if(injective)
        _options |= Rule_InjectiveMatching;
    else
        _options &= ~Rule_InjectiveMatching;
}

/*
void Rule::addVariables(param_t &variables)
{
    _variables.push_back(variables);    

    qDebug() << "rule.cpp: Variables modified";
    _status = Modified;
    emit statusChanged(_status);
}
*/


void Rule::removeVariable(std::string &variable)
{
    for (std::vector<param_t>::iterator it = _variables.begin(); it != _variables.end(); ++it)
    {
      std::vector<std::string> varlist = it->variables;
      for (std::vector<std::string>::iterator itt = varlist.begin(); itt != varlist.end(); ++itt)
      {
          std::string var = *itt;

          // We found the variable to remove
          if (var == variable)
          {
              varlist.erase(itt);
              --itt;
          }
      }    
    }  
    

    // qDebug() << "rule.cpp: Variable modified: " << QVariant(variable.c_str()).toString();

    // Right now variables are inferred, no need to emit a modify
    //_status = Modified;
    //emit statusChanged(_status);
}

bool Rule::save()
{
    // Some initial sanity checks
    if(_path.isEmpty() || !_fp->isOpen())
        return false;

    _fp->close();
    ++_internalChanges;
    _fp->open(QFile::Truncate | QFile::WriteOnly);
    qDebug() << "Saving rule file: " << _fp->fileName();

    
    QString saveText = toAlternative();

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

    QString ruleContents = _fp->readAll();
    if(!ruleContents.isEmpty())
    {
        rule_t rule = parseRule(absolutePath());

//        for (std::vector<param_t>::const_iterator it = rule.parameters.begin(); it != rule.parameters.end(); ++it)
//        {
//            param_t v = *it;
//    //        if (v.type != "list")
//    //            continue;
//            qDebug() << "    rule.cpp: File contains" << v.variables.size() << QString(v.type.c_str()) << (( v.variables.size() > 1 ) ? "variables" : "variable");
//            for (std::vector<std::string>::const_iterator itt = v.variables.begin(); itt != v.variables.end(); ++itt)
//            {
//                qDebug() << "    rule.cpp: (*)" << QString(itt->c_str());
//            }
//        }

        setVariables(rule.parameters);
        emit redrawVariables();
    }

    _status = Normal;
    emit statusChanged(_status);
    return true;
}

// Construct the save file, this means making the documentation into a
// comment and then concatenating the rule contents
QString Rule::toAlternative()
{
    QString saveText = QString();
    QString docText = _documentation;
    docText.replace("\n","\n \\\\ ");

    if (!_documentation.isEmpty())
        saveText += QString("// ") + docText + QString("\n");

    saveText += _name + "\n(\n";

    // Collect variables here
		
    for (std::vector<param_t>::iterator it = _variables.begin(); it != _variables.end(); ++it)
    {
        param_t varList = *it; 	// This is a list of variables of the same type
        //qDebug() << "     " << varList.type.c_str() <<": " ;
        for (std::vector<std::string>::iterator itt = varList.variables.begin();
                                               itt != varList.variables.end();
                                               ++itt )
        {
            std::string var = *itt;
            //qDebug() << "          " << var.c_str();
            saveText += var.c_str();
            if (itt != varList.variables.end() - 1)
                saveText += ", ";
            else
                {
                    saveText += " : ";
                    saveText+= varList.type.c_str() ;
							
                    if (it != (_variables.end() - 1)) saveText += ";";
                    saveText += "\n";
                }
        }
    }
		
    saveText += ")\n";
    saveText += _lhs->toAlternative();
    saveText += "\n=>\n";
    saveText += _rhs->toAlternative();
    saveText += "\n";

    //  Interface
    saveText += "interface = \n{\n";

    for (std::vector<std::string>::iterator i = _interface.elements.begin();
                                            i != _interface.elements.end();
                                            ++i)
    {
        //qDebug() << i->c_str() << " ";
        saveText += i->c_str();
        if ( i != (_interface.elements.end() - 1)) saveText+= ", ";
    }

    saveText += "\n}\n";
    if (!_condition.isEmpty() &&  (_condition != QString(" ")))
    {
        saveText+= "where ";
        saveText += _condition;
        saveText += "\n";
    }
	
    //qDebug() << "    Saving rule condition: " << _condition;


    return saveText;
}

bool Rule::saveAs(const QString &filePath)
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
                    tr("Save Rule As..."),
                    dirPath,
                    tr("GP Rules (*.gpr)"));
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
        qDebug() << "Rule could not be saved to " << filePath;
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

bool Rule::open()
{
    if(!GPFile::open())
        return false;

    qDebug() << "Opening rule file: " << _path;

    if ( (_fp == 0) ||  !_fp->exists())
    {
        qDebug() << "    Rule file does not exist." << _path;
        return false;
    }

    QString ruleContents = _fp->readAll();
    if(ruleContents.isEmpty())
        return false;

    //std::string ruleString = ruleContents.toStdString();

    rule_t rule = parseRule(absolutePath());

    qDebug() << "    Finished parsing rule file. " ;


    setName(rule.id.c_str());

    QString docString = rule.documentation.c_str();
    // Strip opening whitespace and the first * if one exists, this allows for
    // common C/C++/Java-style multiline comments such as the top of this file
    docString.replace(QRegExp("\n\\s*\\*\\s*"), "\n");
    docString = docString.trimmed();
    setDocumentation(docString);

		/*
    if(rule.lhs != NULL)
        setLhs(new Graph(rule.lhs, this));
    else
        setLhs(new Graph(QString(), false, this));
    if(rule.rhs != NULL)
        setRhs(new Graph(rule.rhs, this)); 
    else
        setRhs(new Graph(QString(), false, this));*/

    Graph* lhs = new Graph(rule.lhs, this, true);
    Graph* rhs = new Graph(rule.rhs, this, true);
    setLhs(lhs);
    setRhs(rhs);

    _interface = rule.interface;

    std::vector<std::string> elements = _interface.elements;
    for(std::vector<std::string>::iterator it = elements.begin(); it != elements.end() ; ++it)
    {
        QString var = QString::fromStdString(*it);
        if(_lhs->containsNode(var))
        {
            Node* node = _lhs->node(var);
            node->setIsInterface(true);
        }
        else
            qDebug() << "Warning: Node" << var << "is not contained in LHS of rule"  << _name <<", but it is in the interface";

        if(_rhs->containsNode(var))
        {
            Node* node = _rhs->node(var);
            node->setIsInterface(true);
        }
        else
            qDebug() << "Warning: Node" << var << "is not contained in RHS of rule"  << _name <<", but it is in the interface";
    }

//    for (std::vector<param_t>::const_iterator it = rule.parameters.begin(); it != rule.parameters.end(); ++it)
//    {
//        param_t v = *it;
////        if (v.type != "list")
////            continue;
//        qDebug() << "    rule.cpp: Variables of type" << QString(v.type.c_str());
//        for (std::vector<std::string>::const_iterator itt = v.variables.begin(); itt != v.variables.end(); ++itt)
//        {
//            qDebug() << "    rule.cpp: Variable:" << QString(itt->c_str());
//        }
//    }


    setVariables(rule.parameters);

		/*
    if(rule.condition)
        setCondition(rule.condition.c_str());
    else
        setCondition(QString());*/
    // qDebug() << "  rule.cpp: Setting condition to:" << QString(rule.condition.c_str());
    setCondition(QString(rule.condition.c_str()));

    _status = Normal;
    emit statusChanged(_status);

    return true;
}

void Rule::lhsGraphChanged()
{
    _status = Modified;
    emit statusChanged(_status);
}

void Rule::rhsGraphChanged()
{
    _status = Modified;
    emit statusChanged(_status);
}

void Rule::interfaceChanged()
{
    _status = Modified;
    emit statusChanged(_status);
}

}
