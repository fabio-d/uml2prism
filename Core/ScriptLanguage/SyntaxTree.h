#ifndef CORE_SCRIPTLANGUAGE_SYNTAXTREE_H
#define CORE_SCRIPTLANGUAGE_SYNTAXTREE_H

#include <QList>
#include <QString>

namespace Core
{
namespace ScriptLanguage
{
namespace SyntaxTree
{

class Expression
{
	public:
		virtual ~Expression();

		virtual QString toString() const = 0;
};

class Identifier : public Expression
{
};

class GlobalIdentifier : public Identifier
{
	public:
		explicit GlobalIdentifier(const QString &name);

		QString toString() const override;

	private:
		QString m_name;
};

class MemberIdentifier : public Identifier
{
	public:
		MemberIdentifier(Identifier *parent, const QString &name);

		QString toString() const override;

	private:
		Identifier *m_parent;
		QString m_name;
};

class BoolLiteral : public Expression
{
	public:
		explicit BoolLiteral(bool value);

		QString toString() const override;

	private:
		bool m_value;
};

class NotOperator : public Expression
{
	public:
		NotOperator(Expression *arg);

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

		BinaryOperator(Operator op, Expression *arg1, Expression *arg2);
		~BinaryOperator() override;

		QString toString() const override;

	private:
		Operator m_op;
		Expression *m_arg1, *m_arg2;
};

class Tuple : public Expression
{
	public:
		Tuple(); // Empty tuple
		~Tuple() override;

		void appendElement(Expression *expr);
		QList<Expression*> takeElements();

		QString toString() const override;

	private:
		QList<Expression*> m_elements;
};

class MethodCall : public Expression
{
	public:
		explicit MethodCall(Identifier *method); // no args
		MethodCall(Identifier *method, Tuple *args);
		~MethodCall() override;

		QString toString() const override;

	private:
		Identifier *m_method;
		QList<Expression*> m_arguments;
};

}
}
}

#endif // CORE_SCRIPTLANGUAGE_SYNTAXTREE_H
