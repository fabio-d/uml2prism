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
