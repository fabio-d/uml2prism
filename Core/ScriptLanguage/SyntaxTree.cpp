#include "Core/ScriptLanguage/SyntaxTree.h"

#include <QStringList>

namespace Core
{
namespace ScriptLanguage
{
namespace SyntaxTree
{

Expression::~Expression()
{
}

GlobalIdentifier::GlobalIdentifier(const QString &name)
: m_name(name)
{
}

QString GlobalIdentifier::toString() const
{
	return QString("GlobalIdentifier(\"%1\")").arg(m_name);
}

MemberIdentifier::MemberIdentifier(Identifier *parent, const QString &name)
: m_parent(parent), m_name(name)
{
}

QString MemberIdentifier::toString() const
{
	return QString("MemberIdentifier(%1, \"%2\")").arg(m_parent->toString()).arg(m_name);
}

BoolLiteral::BoolLiteral(bool value)
: m_value(value)
{
}

QString BoolLiteral::toString() const
{
	return QString("BoolLiteral(%1)").arg(m_value ? "true" : "false");
}

NotOperator::NotOperator(Expression *arg)
: m_arg(arg)
{
}

QString NotOperator::toString() const
{
	return QString("NotOperator(%1)")
		.arg(m_arg->toString());
}

BinaryOperator::BinaryOperator(Operator op, Expression *arg1, Expression *arg2)
: m_op(op), m_arg1(arg1), m_arg2(arg2)
{
}

BinaryOperator::~BinaryOperator()
{
	delete m_arg1;
	delete m_arg2;
}

QString BinaryOperator::toString() const
{
	QString opStr;

	switch (m_op)
	{
		case Equal:
			opStr = "Equal";
			break;
		case NotEqual:
			opStr = "NotEqual";
			break;
		case And:
			opStr = "And";
			break;
		case Or:
			opStr = "Or";
			break;
	}

	return QString("BinaryOperator(%1, %2, %3)")
		.arg(opStr)
		.arg(m_arg1->toString())
		.arg(m_arg2->toString());
}

Tuple::Tuple()
{
}

Tuple::~Tuple()
{
	qDeleteAll(m_elements);
}

void Tuple::appendElement(Expression *expr)
{
	m_elements.append(expr);
}

QList<Expression*> Tuple::takeElements()
{
	QList<Expression*> res;
	m_elements.swap(res);
	return res;
}

QString Tuple::toString() const
{
	QStringList elementsStr;

	elementsStr.append(QString::number(m_elements.count()));

	foreach (const Expression *e, m_elements)
		elementsStr.append(e->toString());

	return QString("Tuple(%1)").arg(elementsStr.join(", "));
}

MethodCall::MethodCall(Identifier *method)
: m_method(method)
{
}

MethodCall::MethodCall(Identifier *method, Tuple *args)
: m_method(method), m_arguments(args->takeElements())
{
	delete args;
}

MethodCall::~MethodCall()
{
	qDeleteAll(m_arguments);
}

QString MethodCall::toString() const
{
	QStringList elementsStr;

	elementsStr.append(m_method->toString());
	elementsStr.append(QString::number(m_arguments.count()));

	foreach (const Expression *e, m_arguments)
		elementsStr.append(e->toString());

	return QString("MethodCall(%1)").arg(elementsStr.join(", "));
}

}
}
}
