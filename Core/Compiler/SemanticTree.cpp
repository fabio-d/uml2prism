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

}
}
}
