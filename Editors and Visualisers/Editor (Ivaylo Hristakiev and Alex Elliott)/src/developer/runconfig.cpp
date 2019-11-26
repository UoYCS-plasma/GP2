/*!
 * \file
 */
#include "runconfig.hpp"

namespace Developer {

RunConfig::RunConfig(QObject *parent, const QString& name, const QString& program, const QString& graph)
    : QObject(parent)
		, _name(name)
		, _program(program)
		, _graph(graph)
		, _tracing(false)
		, _backtracking(false)
{
}

RunConfig::~RunConfig()
{

}

QString RunConfig::name()
{
	return _name;
}
QString RunConfig::program()
{
	return _program;
}
QString RunConfig::graph()
{
	return _graph;
}

bool RunConfig::getTracing()
{
	return _tracing;
}
bool RunConfig::getBacktracking()
{
	return _backtracking;
}

void RunConfig::setName(QString name)
{
	_name = name;
}

void RunConfig::setProgram(QString program)
{
	_program = program;
}

void RunConfig::setGraph(QString graph)
{
	_graph = graph;
}

void RunConfig::setTracing(bool tracing)
{
	_tracing = tracing;
}
void RunConfig::setBacktracking(bool backtracking)
{
	_backtracking = backtracking;
}

bool RunConfig::hasTracing()
{
	return _tracing;
}
bool RunConfig::hasBacktracking()
{
	return _backtracking;
}

}
