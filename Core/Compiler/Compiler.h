/*
 * Copyright (C) 2016 Fabio D'Urso
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

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
class UMLScriptedNodeElement;

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

		QString compileInitialNode(const UMLInitialNode *node,
			QString *out_declLine);
		QString compileFlowFinalNode(const UMLFlowFinalNode *node,
			QString *out_declLine);
		QString compileActivityFinalNode(const UMLActivityFinalNode *node,
			QString *out_declLine,
			const QStringList &allStates,
			const QMap<QString, const SemanticTree::Expr *> &restartVarValues,
			const QMap<QString, const SemanticTree::Type *> &restartSignalTypes);
		QString compileActionNode(const UMLActionNode *node,
			QString *out_declLine,
			const SemanticTree::Stmt *script,
			bool branchEnabled,
			ErrorList *out_errorList);
		QString compileDecisionMergeNode(const UMLDecisionMergeNode *node,
			QString *out_declLine,
			const SemanticTree::Stmt *script,
			bool branchEnabled,
			ErrorList *out_errorList);
		QString compileForkJoinNode(const UMLForkJoinNode *node,
			QString *out_declLine,
			int *out_maxValue);

		QString compileLabel(const QString &name, const SemanticTree::Expr *pred);
		QString compileProperty(const QString &name, const SemanticTree::Expr *pred);

	private:
		QString escapeString(const QString &str);
		QString compileScriptedNode(const UMLScriptedNodeElement *node,
			const SemanticTree::Stmt *script,
			bool branchEnabled,
			ErrorList *out_errorList);

		const SemanticContext *m_context;
};

}
}

#endif // CORE_COMPILER_COMPILER_H
