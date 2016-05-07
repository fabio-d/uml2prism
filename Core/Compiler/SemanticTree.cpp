#include "Core/Compiler/SemanticTree.h"

namespace Core
{
namespace Compiler
{
namespace SemanticTree
{

void Type::fillReferencedTypes(QSet<const Type*> &target) const
{
}

const QString BoolType::datatypeName() const
{
	return "bool";
}

EnumerationType::EnumerationType(const QString &datatypeName)
: m_datatypeName(datatypeName)
{
}

void EnumerationType::registerValue(const QString &valueName)
{
	m_values.append(valueName);
}

const QString EnumerationType::datatypeName() const
{
	return m_datatypeName;
}

ClassType::ClassType(const QString &datatypeName)
: m_datatypeName(datatypeName)
{
}

void ClassType::registerMemberVariable(const QString &variableName, const Type *type)
{
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

}
}
}
