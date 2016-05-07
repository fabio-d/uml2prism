#include "Core/Compiler/SyntaxTreeGenerator.h"

#include "Core/Compiler/Lexer.h"
#include "Core/Compiler/Parser.h"

#include <QDebug>

#include <sstream>

namespace Core
{
namespace Compiler
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
			lexer.injectToken(Core::Compiler::Parser::token::START_SCRIPT);
			break;
		case Value:
			lexer.injectToken(Core::Compiler::Parser::token::START_VALUE);
			break;
	}

	Parser parser(lexer, this);
	//parser.set_debug_level(true);
	if (parser.parse() != 0 && m_success)
		setError(SourceLocation(), "Unknown error");
}

SyntaxTreeGenerator::~SyntaxTreeGenerator()
{
	while (m_allNodes.isEmpty() == false)
	{
		SyntaxTree::GarbageCollectible *gcNode = *m_allNodes.begin();
		//qDebug() << "GCing" << gcNode;
		m_allNodes.remove(gcNode);
		delete gcNode;
	}
}

bool SyntaxTreeGenerator::success() const
{
	return m_success;
}

const SourceLocation &SyntaxTreeGenerator::errorLocation() const
{
	Q_ASSERT(m_success == false);
	return m_errorLocation;
}

const QString &SyntaxTreeGenerator::errorMessage() const
{
	Q_ASSERT(m_success == false);
	return m_errorMessage;
}

SyntaxTree::Statement *SyntaxTreeGenerator::resultScript()
{
	Q_ASSERT(m_success == true);
	return m_resultScript;
}

SyntaxTree::Expression *SyntaxTreeGenerator::resultValue()
{
	Q_ASSERT(m_success == true);
	return m_resultValue;
}

void SyntaxTreeGenerator::setError(const SourceLocation &location, const QString &message)
{
	// setError must not be called more than once
	Q_ASSERT(m_success == true);

	m_errorLocation = location;
	m_errorMessage = message;
	m_success = false;

	qDebug() << "SyntaxTreeGenerator failed at" << location.toString() << ":" << message;
}

void SyntaxTreeGenerator::setResultScript(SyntaxTree::Statement *expr)
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
