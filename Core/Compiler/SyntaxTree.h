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
		MemberIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *container, const QString &name);

		const Identifier *container() const;

		QString toString() const override;

	private:
		Identifier *m_container;
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
		NotOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, Expression *arg);

		const Expression *arg() const;

		QString toString() const override;

	private:
		Expression *m_arg;
};

class BinaryOperator : public Expression
{
	public:
		enum Operator
		{
			Equal,
			NotEqual,
			And,
			Or
		};

		BinaryOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, Operator op, Expression *arg1, Expression *arg2);

		Operator op() const;
		const Expression *arg1() const;
		const Expression *arg2() const;

		QString toString() const override;

	private:
		Operator m_op;
		Expression *m_arg1, *m_arg2;
};

class Tuple : public Expression
{
	public:
		Tuple(SyntaxTreeGenerator *owner, const SourceLocation &location, const QList<Expression*> &elements = QList<Expression*>());

		const QList<Expression*> &elements() const;

		QString toString() const override;

	private:
		QList<Expression*> m_elements;
};

class CompoundStatement : public Statement
{
	public:
		CompoundStatement(SyntaxTreeGenerator *owner, const SourceLocation &location, const QList<Statement*> &statements = QList<Statement*>());

		QString toString() const override;

	private:
		QList<Statement*> m_statements;
};

class MethodCall : public Expression, public Statement
{
	public:
		MethodCall(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *method, const QList<Expression*> &arguments = QList<Expression*>());

		const Identifier *object() const; // nullptr if "global method" (i.e. a function)
		const QString &methodName() const;
		const QList<Expression*> &arguments() const;

		// Convenience method to resolve ambiguity between
		// Expression::location() and Statement::location()
		const SourceLocation &location() const;

		QString toString() const override;

	private:
		Identifier *m_method;
		QList<Expression*> m_arguments;
};

class Assignment : public Statement
{
	public:
		Assignment(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *dest, Expression *value);

		QString toString() const override;

	private:
		Identifier *m_dest;
		Expression *m_value;
};

class SignalEmission : public Statement
{
	public:
		SignalEmission(SyntaxTreeGenerator *owner, const SourceLocation &location, GlobalIdentifier *signal, Expression *value = nullptr);

		QString toString() const override;

	private:
		GlobalIdentifier *m_signal;
		Expression *m_value;
};

class IfElse : public Statement
{
	public:
		IfElse(SyntaxTreeGenerator *owner, const SourceLocation &location, Expression *condition, Statement *trueBranch, Statement *falseBranch);

		QString toString() const override;

	private:
		Expression *m_condition;
		Statement *m_trueBranch, *m_falseBranch;
};

class ChoiceOr : public Statement
{
	public:
		ChoiceOr(SyntaxTreeGenerator *owner, const SourceLocation &location, Statement *alt1, Statement *alt2);

		QString toString() const override;

	private:
		Statement *m_alt1, *m_alt2;
};

class Branch : public Statement
{
	public:
		Branch(SyntaxTreeGenerator *owner, const SourceLocation &location, const QString &label);

		QString toString() const override;

	private:
		QString m_label;
};

}
}
}

#endif // CORE_COMPILER_SYNTAXTREE_H
