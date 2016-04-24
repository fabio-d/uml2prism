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
: UMLNodeElement(UMLElementType::InitialNode, "InitialNode")
{
}

UMLFinalNode::UMLFinalNode()
: UMLNodeElement(UMLElementType::FinalNode, "FinalNode")
{
}

UMLActionNode::UMLActionNode()
: UMLNodeElement(UMLElementType::ActionNode, "ActionNode")
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

UMLForkNode::UMLForkNode()
: UMLNodeElement(UMLElementType::ForkNode, "ForkNode")
{
}

UMLJoinNode::UMLJoinNode()
: UMLNodeElement(UMLElementType::JoinNode, "JoinNode")
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

UMLClass::UMLClass()
: UMLElement(UMLElementType::Class), m_className("ClassName")
{
}

void UMLClass::setClassName(const QString &newName)
{
	m_className = newName;
}

const QString &UMLClass::className() const
{
	return m_className;
}

}
