#ifndef CORE_COMPILER_SEMANTICTREEGENERATOR_H
#define CORE_COMPILER_SEMANTICTREEGENERATOR_H

#include "Core/Compiler/SemanticTree.h"
#include "Core/Compiler/SyntaxTreeGenerator.h"

#include <QSet>
#include <QStringList>

namespace Core
{
namespace Compiler
{
class SemanticContext;

class SemanticTreeGenerator
{
	public:
		// Parse a value
		SemanticTreeGenerator(const QString &sourceCode, const SemanticTree::Type *valueType, const SemanticContext *context);

		~SemanticTreeGenerator();

		bool success() const;

	private:
		void setError(const SourceLocation &location, const QString &message);
		void setUnexpectedTypeError(const SourceLocation &location, const SemanticTree::Type *expectedType, const SemanticTree::Type *actualType);

		const SemanticTree::Identifier *resolveIdentifier(const SyntaxTree::Identifier *ident);
		const SemanticTree::Type *deduceType(const SyntaxTree::Expression *expression);
		const SemanticTree::Identifier *expectSetMethod(const SyntaxTree::MethodCall *mCall, const QString &methodName);

		const SemanticTree::Expr *convertExpression(const SyntaxTree::Expression *expression, const SemanticTree::Type *expectedType);
		//const SemanticTree::TODO *convertStatement(const SyntaxTree::Statement *statement);

		bool m_success;
		SourceLocation m_errorLocation;
		QString m_errorMessage;

		const SemanticContext *m_context;
};

}
}

#endif // CORE_COMPILER_SEMANTICTREEGENERATOR_H
