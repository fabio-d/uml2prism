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

#ifndef CORE_COMPILER_SEMANTICTREE_H
#define CORE_COMPILER_SEMANTICTREE_H

#include "Core/Compiler/Bindings_stub.h"

#include <QSet>
#include <QStringList>

namespace Core
{
namespace Compiler
{
namespace SemanticTree
{

class Type
{
	public:
		virtual ~Type();

		virtual const QString datatypeName() const = 0;
		virtual void fillReferencedTypes(QSet<const Type*> &target) const;

		HsStablePtr haskellHandle() const;

	protected:
		Type();
		virtual HsStablePtr createHaskellHandle() const = 0;
		bool hasHaskellHandleBeenCreated() const;

	private:
		mutable HsStablePtr m_haskellHandle;
};

class BoolType : public Type
{
	public:
		const QString datatypeName() const override;

	protected:
		HsStablePtr createHaskellHandle() const override;
};

class EnumerationType : public Type
{
	public:
		explicit EnumerationType(const QString &datatypeName);

		void registerValue(const QString &valueName);
		const QStringList &values() const;

		const QString datatypeName() const override;

	protected:
		HsStablePtr createHaskellHandle() const override;

	private:
		QString m_datatypeName;
		QStringList m_values;
};

class ClassType : public Type
{
	public:
		explicit ClassType(const QString &datatypeName);

		void registerMemberVariable(const QString &variableName, const Type *type);
		QStringList memberVariables() const;
		const Type *findMemberVariable(const QString &name) const;

		const QString datatypeName() const override;
		void fillReferencedTypes(QSet<const Type*> &target) const override;

	protected:
		HsStablePtr createHaskellHandle() const override;

	private:
		QString m_datatypeName;
		QList<QPair<QString, const Type*>> m_memberVariables;
};

class SetType : public Type
{
	public:
		explicit SetType(const Type *innerType);

		const Type *innerType() const;

		const QString datatypeName() const override;
		void fillReferencedTypes(QSet<const Type*> &target) const override;

	protected:
		HsStablePtr createHaskellHandle() const override;

	private:
		const Type *m_innerType;
};

class Idnt
{
	public:
		explicit Idnt(const Type *type);
		virtual ~Idnt();

		HsStablePtr haskellHandle() const;
		const Type *type() const;

		virtual QString toString() const = 0;

	protected:
		HsStablePtr m_haskellHandle;

	private:
		const Type *m_type;
};

class IdntGlobal : public Idnt
{
	public:
		IdntGlobal(const QString &name, const Type *type);

		QString toString() const override;

	private:
		QString m_name;
};

class IdntMember : public Idnt
{
	public:
		IdntMember(const Idnt *container, const QString &name, const Type *type);
		~IdntMember() override;

		QString toString() const override;

	private:
		const Idnt *m_container;
		QString m_name;
};

class Expr
{
	public:
		Expr();
		virtual ~Expr();

		HsStablePtr haskellHandle() const;

		virtual QString toString() const = 0;

	protected:
		HsStablePtr m_haskellHandle;
};

class ExprBoolLiteral : public Expr
{
	public:
		explicit ExprBoolLiteral(bool value);

		QString toString() const override;

	private:
		bool m_value;
};

class ExprEnumLiteral : public Expr
{
	public:
		explicit ExprEnumLiteral(const EnumerationType *type); // nil value
		ExprEnumLiteral(const EnumerationType *type, const QString &value);

		QString toString() const override;

	private:
		const EnumerationType *m_type;
		QString m_value;
};

class ExprClassNilLiteral : public Expr
{
	public:
		explicit ExprClassNilLiteral(const ClassType *type);

		QString toString() const override;

	private:
		const ClassType *m_type;
};

class ExprStateCheck : public Expr
{
	public:
		explicit ExprStateCheck(const QString &stateName);

		QString toString() const override;

	private:
		QString m_stateName;
};

class ExprVariable : public Expr
{
	public:
		explicit ExprVariable(const Idnt *identifier);
		~ExprVariable() override;

		QString toString() const override;

	private:
		const Idnt *m_identifier;
};

class ExprBinOp : public Expr
{
	public:
		enum Operator
		{
			Equal,
			NotEqual,
			And,
			Or,
			Implies,
			Iff
		};

		ExprBinOp(Operator op, const Expr *arg1, const Expr *arg2);
		~ExprBinOp() override;

		QString toString() const override;

	private:
		Operator m_op;
		const Expr *m_arg1, *m_arg2;
};

class ExprNotOp : public Expr
{
	public:
		explicit ExprNotOp(const Expr *arg);
		~ExprNotOp() override;

		QString toString() const override;

	private:
		const Expr *m_arg;
};

class ExprUnProp : public Expr
{
	public:
		ExprUnProp(char quantif, char operand, const Expr *arg);
		~ExprUnProp() override;

		QString toString() const override;

	private:
		char m_quantif; // A or E
		char m_operand; // X, F or G
		const Expr *m_arg;
};

class ExprBinProp : public Expr
{
	public:
		ExprBinProp(char quantif, char operand, const Expr *arg1, const Expr *arg2);
		~ExprBinProp() override;

		QString toString() const override;

	private:
		char m_quantif; // A or E
		char m_operand; // U, W or R
		const Expr *m_arg1, *m_arg2;
};

class ExprTuple : public Expr
{
	public:
		ExprTuple(const Type *type, const QList<const Expr*> &args);
		~ExprTuple() override;

		QString toString() const override;

	private:
		const Type *m_type;
		QList<const Expr*> m_args;
};

class ExprSetContains : public Expr
{
	public:
		ExprSetContains(const Idnt *setIdentifier, const Expr *elementToTest);
		~ExprSetContains() override;

		QString toString() const override;

	private:
		const Idnt *m_setIdentifier;
		const Expr *m_elementToTest;
};

class Stmt
{
	public:
		Stmt();
		virtual ~Stmt();

		HsStablePtr haskellHandle() const;

		virtual QString toString() const = 0;

	protected:
		HsStablePtr m_haskellHandle;
};

class StmtCompound : public Stmt
{
	public:
		explicit StmtCompound(const QList<const Stmt*> &statements);
		virtual ~StmtCompound();

		virtual QString toString() const;

	private:
		QList<const Stmt*> m_statements;
};

class StmtSetInsert : public Stmt
{
	public:
		StmtSetInsert(const Idnt *setIdentifier, const Expr *elementToInsert);
		~StmtSetInsert() override;

		QString toString() const override;

	private:
		const Idnt *m_setIdentifier;
		const Expr *m_elementToInsert;
};

class StmtAssignment : public Stmt
{
	public:
		StmtAssignment(const Idnt *dest, const Expr *value);
		~StmtAssignment() override;

		QString toString() const override;

	private:
		const Idnt *m_dest;
		const Expr *m_value;
};

class StmtIfElse : public Stmt
{
	public:
		StmtIfElse(const Expr *cond, const Stmt *ifTrue, const Stmt *ifFalse);
		virtual ~StmtIfElse();

		virtual QString toString() const;

	private:
		const Expr *m_cond;
		const Stmt *m_ifTrue, *m_ifFalse;
};

class StmtChoiceOr : public Stmt
{
	public:
		StmtChoiceOr(const Stmt *alt1, const Stmt *alt2);
		virtual ~StmtChoiceOr();

		virtual QString toString() const;

	private:
		const Stmt *m_alt1, *m_alt2;
};

class StmtBranch : public Stmt
{
	public:
		explicit StmtBranch(const QString &targetNode);

		virtual QString toString() const;

	private:
		QString m_targetNode;
};

}
}
}

#endif // CORE_COMPILER_SEMANTICTREE_H
