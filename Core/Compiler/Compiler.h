#ifndef CORE_COMPILER_COMPILER_H
#define CORE_COMPILER_COMPILER_H

#include "Core/Compiler/SemanticTree.h"

namespace Core
{
class UMLInitialNode;
class UMLFlowFinalNode;
class UMLActivityFinalNode;
class UMLActionNode;
class UMLDecisionMergeNode;
class UMLForkJoinNode;

namespace Compiler
{
class SemanticContext;

class Compiler
{
	public:
		enum class ErrorType { Error, Warning };
		typedef QPair<ErrorType, QString> Error;
		typedef QList<Error> ErrorList;

		Compiler(const SemanticContext *context);

		QString compileVariableDeclaration(const QString &name,
			const SemanticTree::Type *type,
			const SemanticTree::Expr *initialValue, bool isPersistent);

		QString compileSignalDeclaration(const QString &name,
			const SemanticTree::Type *type);

		QString compileInitialNode(const UMLInitialNode *node);
		QString compileFlowFinalNode(const UMLFlowFinalNode *node);
		QString compileActivityFinalNode(const UMLActivityFinalNode *node,
			const QStringList &allStates,
			const QMap<QString, const SemanticTree::Expr *> &restartVarValues,
			const QMap<QString, const SemanticTree::Type *> &restartSignalTypes);
		QString compileActionNode(const UMLActionNode *node,
			const SemanticTree::Stmt *script, const QString &nextNode,
			ErrorList *out_errorList);
		QString compileDecisionMergeNode(const UMLDecisionMergeNode *node,
			const SemanticTree::Stmt *script, const QString &nextNode,
			ErrorList *out_errorList);
		QString compileForkJoinNode(const UMLForkJoinNode *node);

		QString compileLabel(const QString &name, const SemanticTree::Expr *pred);
		QString compileProperty(const QString &name, const SemanticTree::Expr *pred);

	private:
		QString escapeString(const QString &str);

		const SemanticContext *m_context;
};

}
}

#endif // CORE_COMPILER_COMPILER_H
