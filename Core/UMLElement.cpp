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

UMLInitialNode::UMLInitialNode()
: UMLElement(UMLElementType::InitialNode)
{
}

}
