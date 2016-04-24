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

UMLNodeElement::UMLNodeElement(UMLElementType type)
: UMLElement(type)
{
}

UMLNodeElement::~UMLNodeElement()
{
	Q_ASSERT(m_incomingEdges.isEmpty());
	Q_ASSERT(m_outgoingEdges.isEmpty());
}

void UMLNodeElement::setNodeName(const QString &newName)
{
	m_nodeName = newName;
	emit changed();
}

const QString &UMLNodeElement::nodeName() const
{
	return m_nodeName;
}

const QList<UMLEdgeElement*> &UMLNodeElement::incomingEdges() const
{
	return m_incomingEdges;
}

const QList<UMLEdgeElement*> &UMLNodeElement::outgoingEdges() const
{
	return m_outgoingEdges;
}

UMLInitialNode::UMLInitialNode()
: UMLNodeElement(UMLElementType::InitialNode)
{
}

UMLFinalNode::UMLFinalNode()
: UMLNodeElement(UMLElementType::FinalNode)
{
}

UMLActionNode::UMLActionNode()
: UMLNodeElement(UMLElementType::ActionNode)
{
}

UMLDecisionMergeNode::UMLDecisionMergeNode()
: UMLNodeElement(UMLElementType::DecisionMergeNode)
{
}

UMLForkJoinNode::UMLForkJoinNode()
: UMLNodeElement(UMLElementType::ForkJoinNode)
{
}

UMLEdgeElement::UMLEdgeElement(UMLElementType type, UMLNodeElement *from, UMLNodeElement *to)
: UMLElement(type), m_from(from), m_to(to)
{
}

UMLNodeElement *UMLEdgeElement::from() const
{
	return m_from;
}

UMLNodeElement *UMLEdgeElement::to() const
{
	return m_to;
}

UMLControlFlowEdge::UMLControlFlowEdge(UMLNodeElement *from, UMLNodeElement *to)
: UMLEdgeElement(UMLElementType::ControlFlowEdge, from, to)
{
}

UMLSignalEdge::UMLSignalEdge(UMLNodeElement *from, UMLNodeElement *to)
: UMLEdgeElement(UMLElementType::SignalEdge, from, to)
{
}

UMLDatatypeElement::UMLDatatypeElement(UMLElementType type)
: UMLElement(type)
{
}

void UMLDatatypeElement::setDatatypeName(const QString &newName)
{
	m_datatypeName = newName;
}

const QString &UMLDatatypeElement::datatypeName() const
{
	return m_datatypeName;
}

UMLClass::UMLClass()
: UMLDatatypeElement(UMLElementType::Class)
{
}

UMLEnumeration::UMLEnumeration()
: UMLDatatypeElement(UMLElementType::Enumeration)
{
}

}
