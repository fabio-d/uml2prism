#ifndef CORE_COMPILER_SYNTAXTREE_H
#define CORE_COMPILER_SYNTAXTREE_H

#include "Core/Compiler/SourceLocation.h"

#include <QList>
#include <QString>

namespace Core
{
namespace Compiler
{
class SyntaxTreeGenerator;

namespace SyntaxTree
{

enum class NodeType
{
	GlobalIdentifier,
	MemberIdentifier,
	NilLiteral,
	BoolLiteral,
	NotOperator,
	BinaryOperator,
	UnaryProperty,
	BinaryProperty,
	Tuple,
	CompoundStatement,
	MethodCall,
	Assignment,
	SignalEmission,
	IfElse,
	ChoiceOr,
	Branch
};

class Node
{
	public:
		virtual ~Node();

		const SourceLocation &location() const;
		NodeType nodeType() const;

	protected:
		Node(SyntaxTreeGenerator *owner, const SourceLocation &location, NodeType nodeType);

	private:
		SyntaxTreeGenerator *m_owner;
		SourceLocation m_location;
		NodeType m_nodeType;
};

class Expression : public Node
{
	public:
		Expression(SyntaxTreeGenerator *owner, const SourceLocation &location, NodeType nodeType);
		~Expression();

		virtual QString toString() const = 0;
};

class Statement : public Node
{
	public:
		Statement(SyntaxTreeGenerator *owner, const SourceLocation &location, NodeType nodeType);
		~Statement();

		virtual QString toString() const = 0;
};

class Identifier : public Expression
{
	public:
		const QString &name() const;

	protected:
		Identifier(SyntaxTreeGenerator *owner, const SourceLocation &location, NodeType nodeType, const QString &name);

	private:
		QString m_name;
};

class GlobalIdentifier : public Identifier
{
	public:
		GlobalIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, const QString &name);

		QString toString() const override;
};

class MemberIdentifier : public Identifier
{
	public:
		MemberIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, const Identifier *container, const QString &name);

		const Identifier *container() const;

		QString toString() const override;

	private:
		const Identifier *m_container;
};

class NilLiteral : public Expression
{
	public:
		NilLiteral(SyntaxTreeGenerator *owner, const SourceLocation &location);

		QString toString() const override;
};

class BoolLiteral : public Expression
{
	public:
		BoolLiteral(SyntaxTreeGenerator *owner, const SourceLocation &location, bool value);

		bool value() const;

		QString toString() const override;

	private:
		bool m_value;
};

class NotOperator : public Expression
{
	public:
		NotOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, const Expression *arg);

		const Expression *arg() const;

		QString toString() const override;

	private:
		const Expression *m_arg;
};

class BinaryOperator : public Expression
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

		BinaryOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, Operator op, const Expression *arg1, const Expression *arg2);

		Operator op() const;
		const Expression *arg1() const;
		const Expression *arg2() const;

		QString toString() const override;

	private:
		Operator m_op;
		const Expression *m_arg1, *m_arg2;
};

enum class PropertyQuantifier
{
	ForAll,
	Exists
};

class UnaryProperty : public Expression
{
	public:
		enum Operator
		{
			Next,		// 'X'
			Eventually,	// 'F'
			Always		// 'G'
		};

		UnaryProperty(SyntaxTreeGenerator *owner, const SourceLocation &location, PropertyQuantifier quantif, Operator op, const Expression *arg);

		PropertyQuantifier quantifier() const;
		Operator op() const;
		const Expression *arg() const;

		QString toString() const override;

	private:
		PropertyQuantifier m_quantif;
		Operator m_op;
		const Expression *m_arg;
};

class BinaryProperty : public Expression
{
	public:
		enum Operator
		{
			Until,		// 'U'
			WeakUntil,	// 'W'
			Release		// 'R'
		};

		BinaryProperty(SyntaxTreeGenerator *owner, const SourceLocation &location, PropertyQuantifier quantif, Operator op, const Expression *arg1, const Expression *arg2);

		PropertyQuantifier quantifier() const;
		Operator op() const;
		const Expression *arg1() const;
		const Expression *arg2() const;

		QString toString() const override;

	private:
		PropertyQuantifier m_quantif;
		Operator m_op;
		const Expression *m_arg1, *m_arg2;
};

class Tuple : public Expression
{
	public:
		Tuple(SyntaxTreeGenerator *owner, const SourceLocation &location, const QList<const Expression*> &elements = QList<const Expression*>());

		const QList<const Expression*> &elements() const;

		QString toString() const override;

	private:
		QList<const Expression*> m_elements;
};

class CompoundStatement : public Statement
{
	public:
		CompoundStatement(SyntaxTreeGenerator *owner, const SourceLocation &location, const QList<const Statement*> &statements = QList<const Statement*>());

		const QList<const Statement*> &statements() const;

		QString toString() const override;

	private:
		QList<const Statement*> m_statements;
};

class MethodCall : public Expression, public Statement
{
	public:
		MethodCall(SyntaxTreeGenerator *owner, const SourceLocation &location, const Identifier *method, const QList<const Expression*> &arguments = QList<const Expression*>());

		const Identifier *object() const; // nullptr if "global method" (i.e. a function)
		const QString &methodName() const;
		const QList<const Expression*> &arguments() const;

		// Convenience method to resolve ambiguity between
		// Expression::location() and Statement::location()
		const SourceLocation &location() const;

		QString toString() const override;

	private:
		const Identifier *m_method;
		QList<const Expression*> m_arguments;
};

class Assignment : public Statement
{
	public:
		Assignment(SyntaxTreeGenerator *owner, const SourceLocation &location, const Identifier *dest, const Expression *value);

		const Identifier *dest() const;
		const Expression *value() const;

		QString toString() const override;

	private:
		const Identifier *m_dest;
		const Expression *m_value;
};

class SignalEmission : public Statement
{
	public:
		SignalEmission(SyntaxTreeGenerator *owner, const SourceLocation &location, const GlobalIdentifier *signal, const Expression *value = nullptr);

		const GlobalIdentifier *signal() const;
		const Expression *value() const;

		QString toString() const override;

	private:
		const GlobalIdentifier *m_signal;
		const Expression *m_value;
};

class IfElse : public Statement
{
	public:
		IfElse(SyntaxTreeGenerator *owner, const SourceLocation &location, const Expression *condition, const Statement *trueBranch, const Statement *falseBranch);

		const Expression *condition() const;
		const Statement *trueBranch() const;
		const Statement *falseBranch() const;

		QString toString() const override;

	private:
		const Expression *m_condition;
		const Statement *m_trueBranch, *m_falseBranch;
};

class ChoiceOr : public Statement
{
	public:
		ChoiceOr(SyntaxTreeGenerator *owner, const SourceLocation &location, const Statement *alt1, const Statement *alt2);

		const Statement *alt1() const;
		const Statement *alt2() const;

		QString toString() const override;

	private:
		const Statement *m_alt1, *m_alt2;
};

class Branch : public Statement
{
	public:
		// Note: label = "$default$" is a special marker for branch statements without a label
		Branch(SyntaxTreeGenerator *owner, const SourceLocation &location, const QString &label);

		const QString &label() const;

		QString toString() const override;

	private:
		QString m_label;
};

}
}
}

#endif // CORE_COMPILER_SYNTAXTREE_H
