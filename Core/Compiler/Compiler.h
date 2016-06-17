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

		QString compileInitialNode(const UMLInitialNode *node);
		QString compileFlowFinalNode(const UMLFlowFinalNode *node);
		QString compileActivityFinalNode(const UMLActivityFinalNode *node);
		QString compileActionNode(const UMLActionNode *node,
			const SemanticTree::Stmt *script, const QString &nextNode,
			ErrorList *out_errorList);
		QString compileDecisionMergeNode(const UMLDecisionMergeNode *node,
			const SemanticTree::Stmt *script, const QString &nextNode,
			ErrorList *out_errorList);
		QString compileForkJoinNode(const UMLForkJoinNode *node);

		QString compilePredicate(const SemanticTree::Expr *pred);

	private:
		const SemanticContext *m_context;
};

}
}

#endif // CORE_COMPILER_COMPILER_H
