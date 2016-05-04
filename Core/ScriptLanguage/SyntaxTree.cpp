#include "Core/ScriptLanguage/SyntaxTree.h"

#include "Core/ScriptLanguage/SyntaxTreeGenerator.h"

#include <QDebug>
#include <QStringList>

namespace Core
{
namespace ScriptLanguage
{
namespace SyntaxTree
{

GarbageCollectible::GarbageCollectible(SyntaxTreeGenerator *owner, const SourceLocation &location)
: m_owner(owner), m_location(location)
{
	qDebug() << "ctor" << this << m_location.toString();
	m_owner->m_allNodes.insert(this);
}

GarbageCollectible::~GarbageCollectible()
{
	qDebug() << "dtor" << this;
	m_owner->m_allNodes.remove(this);
}

const SourceLocation &GarbageCollectible::location() const
{
	return m_location;
}

Expression::Expression(SyntaxTreeGenerator *owner, const SourceLocation &location)
: GarbageCollectible(owner, location)
{
}

Identifier::Identifier(SyntaxTreeGenerator *owner, const SourceLocation &location)
: Expression(owner, location)
{
}

GlobalIdentifier::GlobalIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, const QString &name)
: Identifier(owner, location), m_name(name)
{
}

QString GlobalIdentifier::toString() const
{
	return QString("GlobalIdentifier(\"%1\")").arg(m_name);
}

MemberIdentifier::MemberIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *container, const QString &name)
: Identifier(owner, location), m_container(container), m_name(name)
{
}

QString MemberIdentifier::toString() const
{
	return QString("MemberIdentifier(%1, \"%2\")").arg(m_container->toString()).arg(m_name);
}

BoolLiteral::BoolLiteral(SyntaxTreeGenerator *owner, const SourceLocation &location, bool value)
: Expression(owner, location), m_value(value)
{
}

QString BoolLiteral::toString() const
{
	return QString("BoolLiteral(%1)").arg(m_value ? "true" : "false");
}

NotOperator::NotOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, Expression *arg)
: Expression(owner, location), m_arg(arg)
{
}

QString NotOperator::toString() const
{
	return QString("NotOperator(%1)")
		.arg(m_arg->toString());
}

BinaryOperator::BinaryOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, Operator op, Expression *arg1, Expression *arg2)
: Expression(owner, location), m_op(op), m_arg1(arg1), m_arg2(arg2)
{
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

Tuple::Tuple(SyntaxTreeGenerator *owner, const SourceLocation &location)
: Expression(owner, location)
{
}

void Tuple::appendElement(Expression *expr)
{
	m_elements.append(expr);
}

const QList<Expression*> &Tuple::elements() const
{
	return m_elements;
}

QString Tuple::toString() const
{
	QStringList elementsStr;

	elementsStr.append(QString::number(m_elements.count()));

	foreach (const Expression *e, m_elements)
		elementsStr.append(e->toString());

	return QString("Tuple(%1)").arg(elementsStr.join(", "));
}

MethodCall::MethodCall(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *method)
: Expression(owner, location), m_method(method)
{
}

MethodCall::MethodCall(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *method, Tuple *args)
: Expression(owner, location), m_method(method), m_arguments(args->elements())
{
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
