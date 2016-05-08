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

class Identifier
{
	public:
		virtual ~Identifier();

		virtual QString toString() const = 0;
};

class GlobalIdentifier : public Identifier
{
	public:
		GlobalIdentifier(const QString &name, const Type *type);

		QString toString() const override;

	private:
		QString m_name;
		const Type *m_type;
};

class MemberIdentifier : public Identifier
{
	public:
		MemberIdentifier(const Identifier *container, const QString &name, const Type *type);
		~MemberIdentifier() override;

		const Type *type() const;

		QString toString() const override;

	private:
		const Identifier *m_container;
		QString m_name;
		const Type *m_type;
};

class Expr
{
	public:
		virtual ~Expr();

		virtual QString toString() const = 0;
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

class ExprVariable : public Expr
{
	public:
		explicit ExprVariable(const Identifier *identifier);
		~ExprVariable() override;

		QString toString() const override;

	private:
		const Identifier *m_identifier;
};

class ExprBinOp : public Expr
{
	public:
		enum Operator
		{
			Equal,
			NotEqual,
			And,
			Or
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
		explicit ExprSetContains(const Identifier *setIdentifier, const Expr *elementToTest);
		~ExprSetContains() override;

		QString toString() const override;

	private:
		const Identifier *m_setIdentifier;
		const Expr *m_elementToTest;
};

}
}
}

#endif // CORE_COMPILER_SEMANTICTREE_H
