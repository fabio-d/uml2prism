#ifndef CORE_SCRIPTLANGUAGE_SYNTAXTREE_H
#define CORE_SCRIPTLANGUAGE_SYNTAXTREE_H

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

	protected:
		explicit GarbageCollectible(SyntaxTreeGenerator *owner);

	private:
		SyntaxTreeGenerator *m_owner;
};

class Expression : public GarbageCollectible
{
	public:
		explicit Expression(SyntaxTreeGenerator *owner);

		virtual QString toString() const = 0;

	private:
		SyntaxTreeGenerator *m_owner;
};

class Identifier : public Expression
{
	protected:
		explicit Identifier(SyntaxTreeGenerator *owner);
};

class GlobalIdentifier : public Identifier
{
	public:
		GlobalIdentifier(SyntaxTreeGenerator *owner, const QString &name);

		QString toString() const override;

	private:
		QString m_name;
};

class MemberIdentifier : public Identifier
{
	public:
		MemberIdentifier(SyntaxTreeGenerator *owner, Identifier *parent, const QString &name);

		QString toString() const override;

	private:
		Identifier *m_parent;
		QString m_name;
};

class BoolLiteral : public Expression
{
	public:
		BoolLiteral(SyntaxTreeGenerator *owner, bool value);

		QString toString() const override;

	private:
		bool m_value;
};

class NotOperator : public Expression
{
	public:
		NotOperator(SyntaxTreeGenerator *owner, Expression *arg);

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

		BinaryOperator(SyntaxTreeGenerator *owner, Operator op, Expression *arg1, Expression *arg2);

		QString toString() const override;

	private:
		Operator m_op;
		Expression *m_arg1, *m_arg2;
};

class Tuple : public Expression
{
	public:
		explicit Tuple(SyntaxTreeGenerator *owner); // Empty tuple

		void appendElement(Expression *expr);
		const QList<Expression*> &elements() const;

		QString toString() const override;

	private:
		QList<Expression*> m_elements;
};

class MethodCall : public Expression
{
	public:
		MethodCall(SyntaxTreeGenerator *owner, Identifier *method); // no args
		MethodCall(SyntaxTreeGenerator *owner, Identifier *method, Tuple *args);

		QString toString() const override;

	private:
		Identifier *m_method;
		QList<Expression*> m_arguments;
};

}
}
}

#endif // CORE_SCRIPTLANGUAGE_SYNTAXTREE_H
