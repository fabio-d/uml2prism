#include "Core/Compiler/SemanticContext.h"

#include <QDebug>

namespace Core
{
namespace Compiler
{

SemanticContext::SemanticContext()
{
}

SemanticContext::~SemanticContext()
{
	qDeleteAll(m_classAndEnumTypes);
	qDeleteAll(m_setTypes);
}

const SemanticTree::BoolType *SemanticContext::boolType() const
{
	static SemanticTree::BoolType singleton;
	return &singleton;
}

const SemanticTree::Type *SemanticContext::findOtherType(const QString &name) const
{
	return m_classAndEnumTypes.value(name);
}

const SemanticTree::SetType *SemanticContext::findSetType(const SemanticTree::Type *innerType) const
{
	const SemanticTree::SetType *res = m_setTypes.value(innerType, nullptr);

	if (res == nullptr) // create SetType instance if it does not exist yet
	{
		res = new SemanticTree::SetType(innerType);
		m_setTypes.insert(innerType, res);
	}

	return res;
}

SemanticTree::EnumerationType *SemanticContext::registerEnumeration(const QString &enumName)
{
	SemanticTree::EnumerationType *res = new SemanticTree::EnumerationType(enumName);
	m_classAndEnumTypes.insert(enumName, res);
	return res;
}

SemanticTree::ClassType *SemanticContext::registerClass(const QString &className)
{
	SemanticTree::ClassType *res = new SemanticTree::ClassType(className);
	m_classAndEnumTypes.insert(className, res);
	return res;
}

void SemanticContext::registerGlobalVariable(const QString &name, const SemanticTree::Type *type)
{
	m_globalVariables.insert(name, type);
}

void SemanticContext::registerSignal(const QString &name, const SemanticTree::Type *type)
{
	m_signals.insert(name, type);
}

}
}
