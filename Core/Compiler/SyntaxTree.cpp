#include "Core/Compiler/SyntaxTree.h"

#include "Core/Compiler/SyntaxTreeGenerator.h"

#include <QDebug>
#include <QStringList>

namespace Core
{
namespace Compiler
{
namespace SyntaxTree
{

Node::Node(SyntaxTreeGenerator *owner, const SourceLocation &location, NodeType nodeType)
: m_owner(owner), m_location(location), m_nodeType(nodeType)
{
	//qDebug() << "gc ctor" << this << m_location.toString();
	m_owner->m_allNodes.insert(this);
}

Node::~Node()
{
	//qDebug() << "gc dtor" << this;
	m_owner->m_allNodes.remove(this);
}

const SourceLocation &Node::location() const
{
	return m_location;
}

NodeType Node::nodeType() const
{
	return m_nodeType;
}

Expression::Expression(SyntaxTreeGenerator *owner, const SourceLocation &location, NodeType nodeType)
: Node(owner, location, nodeType)
{
	//qDebug() << "expr ctor" << this;
}

Expression::~Expression()
{
	//qDebug() << "expr dtor" << this;
}

Statement::Statement(SyntaxTreeGenerator *owner, const SourceLocation &location, NodeType nodeType)
: Node(owner, location, nodeType)
{
	//qDebug() << "stmt ctor" << this;
}

Statement::~Statement()
{
	//qDebug() << "stmt dtor" << this;
}

Identifier::Identifier(SyntaxTreeGenerator *owner, const SourceLocation &location, NodeType nodeType)
: Expression(owner, location, nodeType)
{
}

GlobalIdentifier::GlobalIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, const QString &name)
: Identifier(owner, location, NodeType::GlobalIdentifier), m_name(name)
{
}

const QString &GlobalIdentifier::name() const
{
	return m_name;
}

QString GlobalIdentifier::toString() const
{
	return QString("GlobalIdentifier(\"%1\")").arg(m_name);
}

MemberIdentifier::MemberIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *container, const QString &name)
: Identifier(owner, location, NodeType::MemberIdentifier), m_container(container), m_name(name)
{
}

const Identifier *MemberIdentifier::container() const
{
	return m_container;
}

const QString &MemberIdentifier::name() const
{
	return m_name;
}

QString MemberIdentifier::toString() const
{
	return QString("MemberIdentifier(%1, \"%2\")").arg(m_container->toString()).arg(m_name);
}

NilLiteral::NilLiteral(SyntaxTreeGenerator *owner, const SourceLocation &location)
: Expression(owner, location, NodeType::NilLiteral)
{
}

QString NilLiteral::toString() const
{
	return QString("NilLiteral");
}

BoolLiteral::BoolLiteral(SyntaxTreeGenerator *owner, const SourceLocation &location, bool value)
: Expression(owner, location, NodeType::BoolLiteral), m_value(value)
{
}

bool BoolLiteral::value() const
{
	return m_value;
}

QString BoolLiteral::toString() const
{
	return QString("BoolLiteral(%1)").arg(m_value ? "true" : "false");
}

NotOperator::NotOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, Expression *arg)
: Expression(owner, location, NodeType::NotOperator), m_arg(arg)
{
}

const Expression *NotOperator::arg() const
{
	return m_arg;
}

QString NotOperator::toString() const
{
	return QString("NotOperator(%1)")
		.arg(m_arg->toString());
}

BinaryOperator::BinaryOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, Operator op, Expression *arg1, Expression *arg2)
: Expression(owner, location, NodeType::BinaryOperator), m_op(op), m_arg1(arg1), m_arg2(arg2)
{
}

BinaryOperator::Operator BinaryOperator::op() const
{
	return m_op;
}

const Expression *BinaryOperator::arg1() const
{
	return m_arg1;
}

const Expression *BinaryOperator::arg2() const
{
	return m_arg2;
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

Tuple::Tuple(SyntaxTreeGenerator *owner, const SourceLocation &location, const QList<Expression*> &elements)
: Expression(owner, location, NodeType::Tuple), m_elements(elements)
{
}

QString Tuple::toString() const
{
	QStringList elementsStr;

	elementsStr.append(QString::number(m_elements.count()));

	foreach (const Expression *e, m_elements)
		elementsStr.append(e->toString());

	return QString("Tuple(%1)").arg(elementsStr.join(", "));
}

CompoundStatement::CompoundStatement(SyntaxTreeGenerator *owner, const SourceLocation &location, const QList<Statement*> &statements)
: Statement(owner, location, NodeType::CompoundStatement), m_statements(statements)
{
}

QString CompoundStatement::toString() const
{
	QStringList statementsStr;

	statementsStr.append(QString::number(m_statements.count()));

	foreach (const Statement *s, m_statements)
		statementsStr.append(s->toString());

	return QString("CompoundStatement(%1)").arg(statementsStr.join(", "));
}

MethodCall::MethodCall(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *method, const QList<Expression*> &arguments)
: Expression(owner, location, NodeType::MethodCall), Statement(owner, location, NodeType::MethodCall), m_method(method), m_arguments(arguments)
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

Assignment::Assignment(SyntaxTreeGenerator *owner, const SourceLocation &location, Identifier *dest, Expression *value)
: Statement(owner, location, NodeType::Assignment), m_dest(dest), m_value(value)
{
}

QString Assignment::toString() const
{
	return QString("Assignment(%1, %2)").arg(m_dest->toString()).arg(m_value->toString());
}

SignalEmission::SignalEmission(SyntaxTreeGenerator *owner, const SourceLocation &location, GlobalIdentifier *signal, Expression *value)
: Statement(owner, location, NodeType::SignalEmission), m_signal(signal), m_value(value)
{
}

QString SignalEmission::toString() const
{
	if (m_value == nullptr)
		return QString("SignalEmission(%1)").arg(m_signal->toString());
	else
		return QString("SignalEmission(%1, %2)").arg(m_signal->toString()).arg(m_value->toString());
}

IfElse::IfElse(SyntaxTreeGenerator *owner, const SourceLocation &location, Expression *condition, Statement *trueBranch, Statement *falseBranch)
: Statement(owner, location, NodeType::IfElse), m_condition(condition), m_trueBranch(trueBranch), m_falseBranch(falseBranch)
{
}

QString IfElse::toString() const
{
	return QString("IfElse(%1, %2, %3)").arg(m_condition->toString()).arg(m_trueBranch->toString()).arg(m_falseBranch->toString());
}

ChoiceOr::ChoiceOr(SyntaxTreeGenerator *owner, const SourceLocation &location, Statement *alt1, Statement *alt2)
: Statement(owner, location, NodeType::ChoiceOr), m_alt1(alt1), m_alt2(alt2)
{
}

QString ChoiceOr::toString() const
{
	return QString("ChoiceOr(%1, %2)").arg(m_alt1->toString()).arg(m_alt2->toString());
}

Branch::Branch(SyntaxTreeGenerator *owner, const SourceLocation &location, const QString &label)
: Statement(owner, location, NodeType::Branch), m_label(label)
{
}

QString Branch::toString() const
{
	return QString("Branch(\"%1\")").arg(m_label);
}

}
}
}
