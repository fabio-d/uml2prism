#include "UMLElement.h"

namespace Core
{

UMLElement::UMLElement(UMLElementType type)
: m_type(type), m_guiProxyPointer(nullptr)
{
}

UMLElement::~UMLElement()
{
}

UMLElementType UMLElement::type() const
{
	return m_type;
}

void UMLElement::setGuiProxyPointer(void *newPointer)
{
	m_guiProxyPointer = newPointer;
}

void *UMLElement::guiProxyPointer() const
{
	return m_guiProxyPointer;
}

void UMLElement::topoSort(QList<UMLElement*> &list, bool reverse)
{
	qSort(list.begin(), list.end(), [=](UMLElement *a, UMLElement *b)
	{
		return ((int)a->type() < (int)b->type()) != reverse;
	});
}

UMLNodeElement::UMLNodeElement(UMLElementType type, const QString &nodeName)
: UMLElement(type), m_nodeName(nodeName)
{
}

void UMLNodeElement::setNodeName(const QString &newName)
{
	m_nodeName = newName;
}

const QString &UMLNodeElement::nodeName() const
{
	return m_nodeName;
}

UMLInitialNode::UMLInitialNode()
: UMLNodeElement(UMLElementType::InitialNode, "InitialNode")
{
}

UMLDecisionNode::UMLDecisionNode()
: UMLNodeElement(UMLElementType::DecisionNode, "DecisionNode")
{
}

UMLMergeNode::UMLMergeNode()
: UMLNodeElement(UMLElementType::MergeNode, "MergeNode")
{
}

}
