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
: m_success(true), m_resultScript(nullptr), m_resultValue(nullptr)
{
	std::istringstream is(sourceCode.toStdString());
	Lexer lexer(&is);

	// Select start symbol
	switch (type)
	{
		case Script:
			lexer.injectToken(Core::ScriptLanguage::Parser::token::START_SCRIPT);
			break;
		case Value:
			lexer.injectToken(Core::ScriptLanguage::Parser::token::START_VALUE);
			break;
	}

	Parser parser(lexer, this);
	//parser.set_debug_level(true);
	if (parser.parse() != 0 && m_success)
		setError(1, 1, "Unknown error");
}

void SyntaxTreeGenerator::setError(int line, int column, const QString &message)
{
	// setError must not be called more than once
	Q_ASSERT(m_success == true);

	m_errorLine = line;
	m_errorColumn = column;
	m_errorMessage = message;
	m_success = false;

	qDebug() << "SyntaxTreeGenerator failed at line" << line << "column" << column << ":" << message;
}

void SyntaxTreeGenerator::setResultScript(SyntaxTree::Expression *expr)
{
	// setError must not have been called
	Q_ASSERT(m_success == true);

	qDebug() << "Got resultScript:" << expr->toString().toLatin1().constData();
	m_resultScript = expr;
}

void SyntaxTreeGenerator::setResultValue(SyntaxTree::Expression *expr)
{
	qDebug() << "Got resultValue:" << expr->toString().toLatin1().constData();
	m_resultValue = expr;
}

}
}
