#ifndef CORE_COMPILER_SEMANTICTREEGENERATOR_H
#define CORE_COMPILER_SEMANTICTREEGENERATOR_H

#include "Core/Compiler/SemanticTree.h"
#include "Core/Compiler/SyntaxTreeGenerator.h"

#include <QMap>
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

		// Parse a statement
		SemanticTreeGenerator(const QString &sourceCode, const SemanticContext *context,
			const QStringList &writableSignals, const QMap<QString, QString> &labelMap);

		~SemanticTreeGenerator();

		bool success() const;
		const SourceLocation &errorLocation() const;
		const QString &errorMessage() const;

		const SemanticTree::Expr *takeResultExpr();
		const SemanticTree::Stmt *takeResultStmt();

	private:
		void setError(const SourceLocation &location, const QString &message);
		void setUnexpectedTypeError(const SourceLocation &location, const SemanticTree::Type *expectedType, const SemanticTree::Type *actualType);

		const SemanticTree::Identifier *resolveIdentifier(const SyntaxTree::Identifier *ident);
		const SemanticTree::Type *deduceType(const SyntaxTree::Expression *expression);
		const SemanticTree::Identifier *expectSetMethod(const SyntaxTree::MethodCall *mCall, const QString &methodName);

		const SemanticTree::Expr *convertExpression(const SyntaxTree::Expression *expression, const SemanticTree::Type *expectedType);
		const SemanticTree::Stmt *convertStatement(const SyntaxTree::Statement *statement);

		bool m_success;
		SourceLocation m_errorLocation;
		QString m_errorMessage;

		const SemanticContext *m_context;

		const SemanticTree::Expr *m_resultExpr;
		const SemanticTree::Stmt *m_resultStmt;

		// The following variables are used only when a statement is
		// being converted
		QStringList m_writableSignals;
		QMap<QString, QString> m_labelMap;
};

}
}

#endif // CORE_COMPILER_SEMANTICTREEGENERATOR_H
