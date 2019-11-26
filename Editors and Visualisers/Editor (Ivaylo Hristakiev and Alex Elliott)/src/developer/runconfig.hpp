/*!
 * \file
 */
#ifndef RUNCONFIG_HPP
#define RUNCONFIG_HPP

#include <QObject>

namespace Developer {

class RunConfig : public QObject
{
    Q_OBJECT

public:
    RunConfig(QObject *parent = 0, const QString& name = QString(""), const QString& program = QString(""), const QString& graph = QString(""));
    ~RunConfig();
    
		QString name();
		QString program();
		QString graph();

		bool getTracing();
		bool getBacktracking();

		void setName(QString name);
		void setProgram(QString program);
		void setGraph(QString graph);

		void setTracing(bool tracing);
		void setBacktracking(bool backtracking);

		bool hasTracing();
		bool hasBacktracking();


signals:
    
public slots:

private:
		QString _name;
		QString _graph;
		QString _program;
		bool _tracing;
		bool _backtracking;    
};

}

#endif // RUNCONFIG_HPP
