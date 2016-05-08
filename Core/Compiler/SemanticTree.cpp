#include "Core/Compiler/SemanticTree.h"

namespace Core
{
namespace Compiler
{
namespace SemanticTree
{

Type::Type()
: m_haskellHandle(nullptr)
{
}

Type::~Type()
{
	if (m_haskellHandle != nullptr)
		hsType_free(m_haskellHandle);
}

void Type::fillReferencedTypes(QSet<const Type*> &target) const
{
}

bool Type::hasHaskellHandleBeenCreated() const
{
	return m_haskellHandle != nullptr;
}

HsStablePtr Type::haskellHandle() const
{
	if (m_haskellHandle == nullptr)
		m_haskellHandle = createHaskellHandle();
	return m_haskellHandle;
}

const QString BoolType::datatypeName() const
{
	return "bool";
}

HsStablePtr BoolType::createHaskellHandle() const
{
	return hsTypeBool_create();
}

EnumerationType::EnumerationType(const QString &datatypeName)
: m_datatypeName(datatypeName)
{
}

void EnumerationType::registerValue(const QString &valueName)
{
	Q_ASSERT(hasHaskellHandleBeenCreated() == false);
	m_values.append(valueName);
}

const QStringList &EnumerationType::values() const
{
	return m_values;
}

const QString EnumerationType::datatypeName() const
{
	return m_datatypeName;
}

HsStablePtr EnumerationType::createHaskellHandle() const
{
	HsStablePtr res = hsTypeEnumeration_create();
	foreach (const QString &val, m_values)
	{
		const QByteArray valArr = val.toLatin1();
		HsStablePtr updated = hsTypeEnumeration_registerValue(res, (void*)valArr.constData());
		hsType_free(res);
		res = updated;
	}
	return res;
}

ClassType::ClassType(const QString &datatypeName)
: m_datatypeName(datatypeName)
{
}

void ClassType::registerMemberVariable(const QString &variableName, const Type *type)
{
	Q_ASSERT(hasHaskellHandleBeenCreated() == false);
	m_memberVariables.append(QPair<QString, const Type*>(variableName, type));
}

QStringList ClassType::memberVariables() const
{
	QStringList res;
	for (QList<QPair<QString, const Type*>>::const_iterator it = m_memberVariables.begin();
		it != m_memberVariables.end(); ++it)
	{
		res.append(it->first);
	}
	return res;
}

const Type *ClassType::findMemberVariable(const QString &name) const
{
	for (QList<QPair<QString, const Type*>>::const_iterator it = m_memberVariables.begin();
		it != m_memberVariables.end(); ++it)
	{
		if (it->first == name)
			return it->second;
	}
	return nullptr;
}

const QString ClassType::datatypeName() const
{
	return m_datatypeName;
}

void ClassType::fillReferencedTypes(QSet<const Type*> &target) const
{
	QSet<const Type*> res;
	for (QList<QPair<QString, const Type*>>::const_iterator it = m_memberVariables.begin();
		it != m_memberVariables.end(); ++it)
	{
		const Type *t = it->second;
		if (target.contains(t))
			continue;

		target.insert(t);
		t->fillReferencedTypes(target);
	}
}

HsStablePtr ClassType::createHaskellHandle() const
{
	HsStablePtr res = hsTypeClass_create();
	for (QList<QPair<QString, const Type*>>::const_iterator it = m_memberVariables.begin();
		it != m_memberVariables.end(); ++it)
	{
		const QByteArray n = it->first.toLatin1();
		const Type *t = it->second;
		HsStablePtr updated = hsTypeClass_registerMemberVariable(res, (void*)n.constData(), t->haskellHandle());
		hsType_free(res);
		res = updated;
	}
	return res;
}

SetType::SetType(const Type *innerType)
: m_innerType(innerType)
{
}

const QString SetType::datatypeName() const
{
	return QString("set of %1").arg(m_innerType->datatypeName());
}

void SetType::fillReferencedTypes(QSet<const Type*> &target) const
{
	target.insert(m_innerType);
	m_innerType->fillReferencedTypes(target);
}

HsStablePtr SetType::createHaskellHandle() const
{
	return hsTypeSet_create(m_innerType->haskellHandle());
}

Identifier::~Identifier()
{
}

GlobalIdentifier::GlobalIdentifier(const QString &name, const Type *type)
: m_name(name), m_type(type)
{
}

QString GlobalIdentifier::toString() const
{
	return m_name;
}

MemberIdentifier::MemberIdentifier(const Identifier *container, const QString &name, const Type *type)
: m_container(container), m_name(name), m_type(type)
{
}

MemberIdentifier::~MemberIdentifier()
{
	delete m_container;
}

const Type *MemberIdentifier::type() const
{
	return m_type;
}

QString MemberIdentifier::toString() const
{
	return QString("%1.%2").arg(m_container->toString()).arg(m_name);
}

Expr::~Expr()
{
}

ExprBoolLiteral::ExprBoolLiteral(bool value)
: m_value(value)
{
}

QString ExprBoolLiteral::toString() const
{
	return QString("ExprBoolLiteral(%1)").arg(m_value ? "true" : "false");
}

ExprEnumLiteral::ExprEnumLiteral(const EnumerationType *type)
: m_type(type)
{
}

ExprEnumLiteral::ExprEnumLiteral(const EnumerationType *type, const QString &value)
: m_type(type), m_value(value)
{
}

QString ExprEnumLiteral::toString() const
{
	if (m_value.isEmpty())
		return QString("ExprEnumLiteral(%1, nil)").arg(m_type->datatypeName());
	else
		return QString("ExprEnumLiteral(%1, %2)").arg(m_type->datatypeName()).arg(m_value);
}

ExprClassNilLiteral::ExprClassNilLiteral(const ClassType *type)
: m_type(type)
{
}

QString ExprClassNilLiteral::toString() const
{
	return QString("ExprClassNilLiteral(%1)").arg(m_type->datatypeName());
}

ExprVariable::ExprVariable(const Identifier *identifier)
: m_identifier(identifier)
{
}

ExprVariable::~ExprVariable()
{
	delete m_identifier;
}

QString ExprVariable::toString() const
{
	return QString("ExprVariable(%1)").arg(m_identifier->toString());
}

ExprBinOp::ExprBinOp(Operator op, const Expr *arg1, const Expr *arg2)
: m_op(op), m_arg1(arg1), m_arg2(arg2)
{
}

ExprBinOp::~ExprBinOp()
{
	delete m_arg1;
	delete m_arg2;
}

QString ExprBinOp::toString() const
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

	return QString("ExprBinOp(%1, %2, %3)")
		.arg(opStr)
		.arg(m_arg1->toString())
		.arg(m_arg2->toString());
}

}
}
}
