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

		virtual QString toString() const = 0;

	private:
		SyntaxTreeGenerator *m_owner;
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
		Tuple(SyntaxTreeGenerator *owner, const SourceLocation &location); // Empty tuple

		void appendElement(Expression *expr);
		const QList<Expression*> &elements() const;

		QString toString() const override;

	private:
		QList<Expression*> m_elements;
};

class MethodCall : public Expression
{
	public:
		MethodCall(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *method); // no args
		MethodCall(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *method, Tuple *args);

		QString toString() const override;

	private:
		Identifier *m_method;
		QList<Expression*> m_arguments;
};

}
}
}

#endif // CORE_SCRIPTLANGUAGE_SYNTAXTREE_H
