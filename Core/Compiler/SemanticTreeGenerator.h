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

		const SemanticTree::Expression *convertExpression(const SyntaxTree::Expression *expression, const SemanticTree::Type *expectedType);

		bool m_success;
		SourceLocation m_errorLocation;
		QString m_errorMessage;

		const SemanticContext *m_context;
};

}
}

#endif // CORE_COMPILER_SEMANTICTREEGENERATOR_H
