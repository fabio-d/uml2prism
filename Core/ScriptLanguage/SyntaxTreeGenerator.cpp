#include "Core/ScriptLanguage/SyntaxTreeGenerator.h"

#include "Core/ScriptLanguage/Lexer.h"
#include "Core/ScriptLanguage/Parser.h"

#include <QDebug>

#include <sstream>

namespace Core
{
namespace ScriptLanguage
{

SyntaxTreeGenerator::SyntaxTreeGenerator(const QString &sourceCode, SourceType type)
: m_success(true)
{
	std::istringstream is(sourceCode.toStdString());
	Lexer lexer(&is);

	Parser parser(lexer, this);
	//parser.set_debug_level(true);
	if (parser.parse() != 0 && m_success)
		setError(1, 1, "Unknown error");
}

void SyntaxTreeGenerator::setError(int line, int column, const QString &message)
{
	// setError should not be called more than once
	Q_ASSERT(m_success == true);

	m_errorLine = line;
	m_errorColumn = column;
	m_errorMessage = message;
	m_success = false;

	qDebug() << "SyntaxTreeGenerator failed at line" << line << "column" << column << ":" << message;
}

}
}
