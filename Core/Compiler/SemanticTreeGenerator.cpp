#include "Core/Compiler/SemanticTreeGenerator.h"

#include "Core/Compiler/SyntaxTreeGenerator.h"

#include <QDebug>

namespace Core
{
namespace Compiler
{

SemanticTreeGenerator::SemanticTreeGenerator(const QString &sourceCode, const SemanticTree::Type *valueType, const SemanticContext *context)
: m_success(true), m_context(context)
{
	SyntaxTreeGenerator sygen(sourceCode, SyntaxTreeGenerator::Value);
	if (!sygen.success())
	{
		setError(sygen.errorLocation(), sygen.errorMessage());
		return;
	}

	convertExpression(sygen.resultValue(), valueType);
}

SemanticTreeGenerator::~SemanticTreeGenerator()
{
}

bool SemanticTreeGenerator::success() const
{
	return m_success;
}

void SemanticTreeGenerator::setError(const SourceLocation &location, const QString &message)
{
	// setError must not be called more than once
	Q_ASSERT(m_success == true);

	m_errorLocation = location;
	m_errorMessage = message;
	m_success = false;

	qDebug() << "SemanticTreeGenerator failed at" << location.toString() << ":" << message;
}

const SemanticTree::Expression *SemanticTreeGenerator::convertExpression(const SyntaxTree::Expression *expression, const SemanticTree::Type *expectedType)
{
	qDebug() << "Converting expression" << expression->toString() << "to type" << expectedType->datatypeName();
	setError(expression->location(), "Not implemented yet");
	return nullptr;
}

}
}
