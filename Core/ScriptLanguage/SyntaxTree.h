#ifndef CORE_SCRIPTLANGUAGE_SYNTAXTREE_H
#define CORE_SCRIPTLANGUAGE_SYNTAXTREE_H

#include "Core/ScriptLanguage/SourceLocation.h"

#include <QList>
#include <QString>

namespace Core
{
namespace ScriptLanguage
{
class SyntaxTreeGenerator;

namespace SyntaxTree
{

class GarbageCollectible
{
	public:
		virtual ~GarbageCollectible();

		const SourceLocation &location() const;

	protected:
		GarbageCollectible(SyntaxTreeGenerator *owner, const SourceLocation &location);

	private:
		SyntaxTreeGenerator *m_owner;
		SourceLocation m_location;
};

class Expression : public GarbageCollectible
{
	public:
		Expression(SyntaxTreeGenerator *owner, const SourceLocation &location);
		~Expression();

		virtual QString toString() const = 0;
};

class Statement : public GarbageCollectible
{
	public:
		Statement(SyntaxTreeGenerator *owner, const SourceLocation &location);
		~Statement();

		virtual QString toString() const = 0;
};

class Identifier : public Expression
{
	protected:
		Identifier(SyntaxTreeGenerator *owner, const SourceLocation &location);
};

class GlobalIdentifier : public Identifier
{
	public:
		GlobalIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, const QString &name);

		QString toString() const override;

	private:
		QString m_name;
};

class MemberIdentifier : public Identifier
{
	public:
		MemberIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *container, const QString &name);

		QString toString() const override;

	private:
		Identifier *m_container;
		QString m_name;
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

		QString toString() const override;

	private:
		bool m_value;
};

class NotOperator : public Expression
{
	public:
		NotOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, Expression *arg);

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

		QString toString() const override;

	private:
		Operator m_op;
		Expression *m_arg1, *m_arg2;
};

class Tuple : public Expression
{
	public:
		Tuple(SyntaxTreeGenerator *owner, const SourceLocation &location, const QList<Expression*> &elements = QList<Expression*>());

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

}
}
}

#endif // CORE_SCRIPTLANGUAGE_SYNTAXTREE_H
