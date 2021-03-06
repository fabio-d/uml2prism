/*
 * Copyright (C) 2016 Fabio D'Urso
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

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

Identifier::Identifier(SyntaxTreeGenerator *owner, const SourceLocation &location, NodeType nodeType, const QString &name)
: Expression(owner, location, nodeType), m_name(name)
{
}

const QString &Identifier::name() const
{
	return m_name;
}

GlobalIdentifier::GlobalIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, const QString &name)
: Identifier(owner, location, NodeType::GlobalIdentifier, name)
{
}

QString GlobalIdentifier::toString() const
{
	return QString("GlobalIdentifier(\"%1\")").arg(name());
}

MemberIdentifier::MemberIdentifier(SyntaxTreeGenerator *owner, const SourceLocation &location, const Identifier *container, const QString &name)
: Identifier(owner, location, NodeType::MemberIdentifier, name), m_container(container)
{
}

const Identifier *MemberIdentifier::container() const
{
	return m_container;
}

QString MemberIdentifier::toString() const
{
	return QString("MemberIdentifier(%1, \"%2\")").arg(m_container->toString()).arg(name());
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

NotOperator::NotOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, const Expression *arg)
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

BinaryOperator::BinaryOperator(SyntaxTreeGenerator *owner, const SourceLocation &location, Operator op, const Expression *arg1, const Expression *arg2)
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
		case Implies:
			opStr = "Implies";
			break;
		case Iff:
			opStr = "Iff";
			break;
	}

	return QString("BinaryOperator(%1, %2, %3)")
		.arg(opStr)
		.arg(m_arg1->toString())
		.arg(m_arg2->toString());
}

UnaryProperty::UnaryProperty(SyntaxTreeGenerator *owner, const SourceLocation &location, PropertyQuantifier quantif, Operator op, const Expression *arg)
: Expression(owner, location, NodeType::UnaryProperty), m_quantif(quantif),
  m_op(op), m_arg(arg)
{
}

PropertyQuantifier UnaryProperty::quantifier() const
{
	return m_quantif;
}

UnaryProperty::Operator UnaryProperty::op() const
{
	return m_op;
}

const Expression *UnaryProperty::arg() const
{
	return m_arg;
}

QString UnaryProperty::toString() const
{
	QString quantifStr, opStr;

	switch (m_quantif)
	{
		case PropertyQuantifier::ForAll:
			quantifStr = "ForAll";
			break;
		case PropertyQuantifier::Exists:
			quantifStr = "Exists";
			break;
	}

	switch (m_op)
	{
		case Next:
			opStr = "Next";
			break;
		case Eventually:
			opStr = "Eventually";
			break;
		case Always:
			opStr = "Always";
			break;
	}

	return QString("UnaryProperty(%1, %2, %3)")
		.arg(quantifStr)
		.arg(opStr)
		.arg(m_arg->toString());
}

BinaryProperty::BinaryProperty(SyntaxTreeGenerator *owner, const SourceLocation &location, PropertyQuantifier quantif, Operator op, const Expression *arg1, const Expression *arg2)
: Expression(owner, location, NodeType::BinaryProperty), m_quantif(quantif),
  m_op(op), m_arg1(arg1), m_arg2(arg2)
{
}

PropertyQuantifier BinaryProperty::quantifier() const
{
	return m_quantif;
}

BinaryProperty::Operator BinaryProperty::op() const
{
	return m_op;
}

const Expression *BinaryProperty::arg1() const
{
	return m_arg1;
}

const Expression *BinaryProperty::arg2() const
{
	return m_arg2;
}

QString BinaryProperty::toString() const
{
	QString quantifStr, opStr;

	switch (m_quantif)
	{
		case PropertyQuantifier::ForAll:
			quantifStr = "ForAll";
			break;
		case PropertyQuantifier::Exists:
			quantifStr = "Exists";
			break;
	}

	switch (m_op)
	{
		case Until:
			opStr = "Until";
			break;
		case WeakUntil:
			opStr = "WeakUntil";
			break;
		case Release:
			opStr = "Release";
			break;
	}

	return QString("BinaryProperty(%1, %2, %3, %4)")
		.arg(quantifStr)
		.arg(opStr)
		.arg(m_arg1->toString())
		.arg(m_arg2->toString());
}

Tuple::Tuple(SyntaxTreeGenerator *owner, const SourceLocation &location, const QList<const Expression*> &elements)
: Expression(owner, location, NodeType::Tuple), m_elements(elements)
{
}

const QList<const Expression*> &Tuple::elements() const
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

CompoundStatement::CompoundStatement(SyntaxTreeGenerator *owner, const SourceLocation &location, const QList<const Statement*> &statements)
: Statement(owner, location, NodeType::CompoundStatement), m_statements(statements)
{
}

const QList<const Statement*> &CompoundStatement::statements() const
{
	return m_statements;
}

QString CompoundStatement::toString() const
{
	QStringList statementsStr;

	statementsStr.append(QString::number(m_statements.count()));

	foreach (const Statement *s, m_statements)
		statementsStr.append(s->toString());

	return QString("CompoundStatement(%1)").arg(statementsStr.join(", "));
}

MethodCall::MethodCall(SyntaxTreeGenerator *owner, const SourceLocation &location, const Identifier *method, const QList<const Expression*> &arguments)
: Expression(owner, location, NodeType::MethodCall), Statement(owner, location, NodeType::MethodCall), m_method(method), m_arguments(arguments)
{
}

const Identifier *MethodCall::object() const
{
	if (m_method->nodeType() == NodeType::GlobalIdentifier)
		return nullptr;

	const MemberIdentifier *mId = static_cast<const MemberIdentifier*>(m_method);
	return mId->container();
}

const QString &MethodCall::methodName() const
{
	return m_method->name();
}

const QList<const Expression*> &MethodCall::arguments() const
{
	return m_arguments;
}

const SourceLocation &MethodCall::location() const
{
	return Expression::location();
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

Assignment::Assignment(SyntaxTreeGenerator *owner, const SourceLocation &location, const Identifier *dest, const Expression *value)
: Statement(owner, location, NodeType::Assignment), m_dest(dest), m_value(value)
{
}

const Identifier *Assignment::dest() const
{
	return m_dest;
}

const Expression *Assignment::value() const
{
	return m_value;
}

QString Assignment::toString() const
{
	return QString("Assignment(%1, %2)").arg(m_dest->toString()).arg(m_value->toString());
}

SignalEmission::SignalEmission(SyntaxTreeGenerator *owner, const SourceLocation &location, const GlobalIdentifier *signal, const Expression *value)
: Statement(owner, location, NodeType::SignalEmission), m_signal(signal), m_value(value)
{
}

const GlobalIdentifier *SignalEmission::signal() const
{
	return m_signal;
}

const Expression *SignalEmission::value() const
{
	return m_value;
}

QString SignalEmission::toString() const
{
	if (m_value == nullptr)
		return QString("SignalEmission(%1)").arg(m_signal->toString());
	else
		return QString("SignalEmission(%1, %2)").arg(m_signal->toString()).arg(m_value->toString());
}

IfElse::IfElse(SyntaxTreeGenerator *owner, const SourceLocation &location, const Expression *condition, const Statement *trueBranch, const Statement *falseBranch)
: Statement(owner, location, NodeType::IfElse), m_condition(condition), m_trueBranch(trueBranch), m_falseBranch(falseBranch)
{
}

const Expression *IfElse::condition() const
{
	return m_condition;
}

const Statement *IfElse::trueBranch() const
{
	return m_trueBranch;
}

const Statement *IfElse::falseBranch() const
{
	return m_falseBranch;
}

QString IfElse::toString() const
{
	return QString("IfElse(%1, %2, %3)").arg(m_condition->toString()).arg(m_trueBranch->toString()).arg(m_falseBranch->toString());
}

ChoiceOr::ChoiceOr(SyntaxTreeGenerator *owner, const SourceLocation &location, const Statement *alt1, const Statement *alt2)
: Statement(owner, location, NodeType::ChoiceOr), m_alt1(alt1), m_alt2(alt2)
{
}

const Statement *ChoiceOr::alt1() const
{
	return m_alt1;
}

const Statement *ChoiceOr::alt2() const
{
	return m_alt2;
}

QString ChoiceOr::toString() const
{
	return QString("ChoiceOr(%1, %2)").arg(m_alt1->toString()).arg(m_alt2->toString());
}

Branch::Branch(SyntaxTreeGenerator *owner, const SourceLocation &location, const QString &label)
: Statement(owner, location, NodeType::Branch), m_label(label)
{
}

const QString &Branch::label() const
{
	return m_label;
}

QString Branch::toString() const
{
	return QString("Branch(\"%1\")").arg(m_label);
}

}
}
}
