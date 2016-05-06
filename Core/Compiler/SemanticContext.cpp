#include "Core/Compiler/SemanticContext.h"

#include <QDebug>

namespace Core
{
namespace Compiler
{

void SemanticContext::Type::fillReferencedTypes(QSet<const Type*> &target) const
{
}

const QString SemanticContext::BoolType::datatypeName() const
{
	return "bool";
}

SemanticContext::EnumerationType::EnumerationType(const QString &datatypeName)
: m_datatypeName(datatypeName)
{
}

void SemanticContext::EnumerationType::registerValue(const QString &valueName)
{
	m_values.append(valueName);
}

const QString SemanticContext::EnumerationType::datatypeName() const
{
	return m_datatypeName;
}

SemanticContext::ClassType::ClassType(const QString &datatypeName)
: m_datatypeName(datatypeName)
{
}

void SemanticContext::ClassType::registerMemberVariable(const QString &variableName, const Type *type)
{
	m_memberVariables.insert(variableName, type);
}

const QString SemanticContext::ClassType::datatypeName() const
{
	return m_datatypeName;
}

void SemanticContext::ClassType::fillReferencedTypes(QSet<const Type*> &target) const
{
	QSet<const SemanticContext::Type*> res;
	foreach (const Type *t, m_memberVariables)
	{
		if (target.contains(t))
			continue;

		target.insert(t);
		t->fillReferencedTypes(target);
	}
}

SemanticContext::SetType::SetType(const Type *innerType)
: m_innerType(innerType)
{
}

const QString SemanticContext::SetType::datatypeName() const
{
	return QString("set of %1").arg(m_innerType->datatypeName());
}

void SemanticContext::SetType::fillReferencedTypes(QSet<const Type*> &target) const
{
	target.insert(m_innerType);
	m_innerType->fillReferencedTypes(target);
}

SemanticContext::SemanticContext()
{
}

SemanticContext::~SemanticContext()
{
	qDeleteAll(m_classAndEnumTypes);
	qDeleteAll(m_setTypes);
}

const SemanticContext::BoolType *SemanticContext::boolType() const
{
	static BoolType singleton;
	return &singleton;
}

const SemanticContext::Type *SemanticContext::findOtherType(const QString &name) const
{
	return m_classAndEnumTypes.value(name);
}

const SemanticContext::SetType *SemanticContext::findSetType(const Type *innerType) const
{
	const SetType *res = m_setTypes.value(innerType, nullptr);

	if (res == nullptr) // create SetType instance if it does not exist yet
	{
		res = new SetType(innerType);
		m_setTypes.insert(innerType, res);
	}

	return res;
}

SemanticContext::EnumerationType *SemanticContext::registerEnumeration(const QString &enumName)
{
	EnumerationType *res = new EnumerationType(enumName);
	m_classAndEnumTypes.insert(enumName, res);
	return res;
}

SemanticContext::ClassType *SemanticContext::registerClass(const QString &className)
{
	ClassType *res = new ClassType(className);
	m_classAndEnumTypes.insert(className, res);
	return res;
}

void SemanticContext::registerGlobalVariable(const QString &name, const Type *type)
{
	m_globalVariables.insert(name, type);
}

}
}
