#include "Core/Compiler/SemanticContext.h"

#include <QDebug>

namespace Core
{
namespace Compiler
{

SemanticContext::~SemanticContext()
{
	qDeleteAll(m_classAndEnumTypes);
	qDeleteAll(m_setTypes);
}

const SemanticTree::BoolType *SemanticContext::boolType() const
{
	return &m_boolType;
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

void SemanticContext::registerState(const QString &name)
{
	m_states.append(name);
}

void SemanticContext::registerLabel(const QString &name)
{
	m_labels.append(name);
}

const SemanticTree::EnumerationType *SemanticContext::findEnumerationValue(const QString &value) const
{
	foreach (const SemanticTree::Type *t, m_classAndEnumTypes)
	{
		const SemanticTree::EnumerationType *e =
			dynamic_cast<const SemanticTree::EnumerationType*>(t);
		if (e != nullptr && e->values().contains(value))
			return e;
	}
	return nullptr;
}

const SemanticTree::Type *SemanticContext::findGlobalVariableOrSignalWithMessage(const QString &name) const
{
	return m_globalVariables.value(name, nullptr) ?:
		m_signals.value(name, nullptr);
}

bool SemanticContext::findStateOrLabelOrSignalWithoutMessage(const QString &name) const
{
	if (m_states.contains(name) || m_labels.contains(name))
		return true;
	if (m_signals.contains(name) && m_signals.value(name) == nullptr)
		return true;
	else
		return false;
}

bool SemanticContext::isSignalWithMessage(const QString &name) const
{
	return m_signals.value(name, nullptr) != nullptr;
}

bool SemanticContext::isSignalWithoutMessage(const QString &name) const
{
	return m_signals.contains(name) && m_signals.value(name) == nullptr;
}

bool SemanticContext::isGlobalVariable(const QString &name) const
{
	return m_globalVariables.contains(name);
}

bool SemanticContext::isState(const QString &name) const
{
	return m_states.contains(name);
}

bool SemanticContext::isLabel(const QString &name) const
{
	return m_labels.contains(name);
}

}
}
