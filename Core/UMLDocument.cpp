#include "Core/UMLDocument.h"

#include "Core/GuiProxy.h"
#include "Core/UMLElement.h"

namespace Core
{

UMLDocument::UMLDocument(Type type)
: m_type(type), m_guiProxy(nullptr)
{
}

UMLDocument::~UMLDocument()
{
	deleteAllElements();
}

UMLDocument::Type UMLDocument::type() const
{
	return m_type;
}

void UMLDocument::setGuiProxy(GuiProxy *guiProxy)
{
	m_guiProxy = guiProxy;
}

GuiProxy *UMLDocument::guiProxy() const
{
	return m_guiProxy;
}

void UMLDocument::addUMLElement(UMLElement *element)
{
	Q_ASSERT(m_elements.contains(element) == false);

	UMLNodeElement *edgeFrom, *edgeTo;
	if (element->type() == UMLElementType::ControlFlowEdge)
	{
		UMLEdgeElement *edge = static_cast<UMLEdgeElement*>(element);
		edgeFrom = edge->from();
		edgeTo = edge->to();
		edgeFrom->m_outgoingEdges.append(edge);
		edgeTo->m_incomingEdges.append(edge);
	}
	else
	{
		edgeFrom = edgeTo = nullptr;
	}

	if (m_guiProxy)
	{
		m_guiProxy->notifyElementAdded(element);

		if (edgeFrom != nullptr)
		{
			Q_ASSERT(edgeTo != nullptr);
			m_guiProxy->notifyElementChanged(edgeFrom);
			m_guiProxy->notifyElementChanged(edgeTo);
		}
	}

	connect(element, SIGNAL(changed()), this, SLOT(slotElementChanged()));
	m_elements.append(element);
}

void UMLDocument::deleteUMLElement(UMLElement *element)
{
	Q_ASSERT(m_elements.contains(element) == true);
	disconnect(element, SIGNAL(changed()), this, SLOT(slotElementChanged()));

	UMLNodeElement *edgeFrom, *edgeTo;
	if (element->type() == UMLElementType::ControlFlowEdge)
	{
		UMLEdgeElement *edge = static_cast<UMLEdgeElement*>(element);
		edgeFrom = edge->from();
		edgeTo = edge->to();
		edgeFrom->m_outgoingEdges.removeOne(edge);
		edgeTo->m_incomingEdges.removeOne(edge);
	}
	else
	{
		edgeFrom = edgeTo = nullptr;
	}

	if (m_guiProxy)
	{
		m_guiProxy->notifyElementRemoved(element);

		if (edgeFrom != nullptr)
		{
			Q_ASSERT(edgeTo != nullptr);
			m_guiProxy->notifyElementChanged(edgeFrom);
			m_guiProxy->notifyElementChanged(edgeTo);
		}
	}

	m_elements.removeOne(element);
	delete element;
}

void UMLDocument::deleteAllElements()
{
	// free elements in reverse order to avoid breaking references
	while (m_elements.size() != 0)
		deleteUMLElement(m_elements.last());
}

void UMLDocument::slotElementChanged()
{
	Core::UMLElement *element = qobject_cast<UMLElement*>(QObject::sender());

	if (m_guiProxy)
		m_guiProxy->notifyElementChanged(element);
}

}
