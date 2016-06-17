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
		// Parse an expression
		SemanticTreeGenerator(const QString &sourceCode, const SemanticContext *context,
			const SemanticTree::Type *valueType, bool allowProperties);

		// Parse a statement
		SemanticTreeGenerator(const QString &sourceCode, const SemanticContext *context,
			const QStringList &writableSignals, const QMap<QString, QString> &labelMap,
			const QString &defaultBranchTarget);

		~SemanticTreeGenerator();

		bool success() const;
		const SourceLocation &errorLocation() const;
		const QString &errorMessage() const;

		const SemanticTree::Expr *takeResultExpr();
		const SemanticTree::Stmt *takeResultStmt();

	private:
		void setError(const SourceLocation &location, const QString &message);
		void setUnexpectedTypeError(const SourceLocation &location, const SemanticTree::Type *expectedType, const SemanticTree::Type *actualType);

		// Some identifiers cannot be used in some context. This value
		// tells resolveIdentifier how the resulting identifier is going
		// to be used
		enum IdentifierPurpose
		{
			Read,
			SendSignalWithoutMessage,
			SendSignalWithMessage,
			WriteVariable
		};

		const SemanticTree::Idnt *resolveIdentifier(const SyntaxTree::Identifier *ident, IdentifierPurpose purpose);
		const SemanticTree::Type *deduceType(const SyntaxTree::Expression *expression);

		enum SetMethod
		{
			Contains,
			Insert
		};

		const SemanticTree::Idnt *expectSetMethod(const SyntaxTree::MethodCall *mCall, SetMethod method);

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
		QString m_defaultBranchTarget;
};

}
}

#endif // CORE_COMPILER_SEMANTICTREEGENERATOR_H
