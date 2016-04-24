#include "Core/UMLDiagram.h"

#include "Core/GuiProxy.h"
#include "Core/UMLElement.h"

namespace Core
{

UMLDiagram::UMLDiagram(Type type)
: m_type(type), m_guiProxy(nullptr)
{
}

UMLDiagram::~UMLDiagram()
{
	deleteAllElements();
}

UMLDiagram::Type UMLDiagram::type() const
{
	return m_type;
}

void UMLDiagram::setGuiProxy(GuiProxy *guiProxy)
{
	m_guiProxy = guiProxy;
}

GuiProxy *UMLDiagram::guiProxy() const
{
	return m_guiProxy;
}

void UMLDiagram::addUMLElement(UMLElement *element)
{
	Q_ASSERT(m_elements.contains(element) == false);

	UMLEdgeElement *elementAsEdge = dynamic_cast<UMLEdgeElement*>(element);
	if (elementAsEdge != nullptr)
	{
		elementAsEdge->from()->m_outgoingEdges.append(elementAsEdge);
		elementAsEdge->to()->m_incomingEdges.append(elementAsEdge);
	}

	if (m_guiProxy)
	{
		m_guiProxy->notifyElementAdded(element);

		if (elementAsEdge != nullptr)
		{
			m_guiProxy->notifyElementChanged(elementAsEdge->from());
			m_guiProxy->notifyElementChanged(elementAsEdge->to());
		}
	}

	connect(element, SIGNAL(changed()), this, SLOT(slotElementChanged()));
	m_elements.append(element);
}

void UMLDiagram::deleteUMLElement(UMLElement *element)
{
	Q_ASSERT(m_elements.contains(element) == true);
	disconnect(element, SIGNAL(changed()), this, SLOT(slotElementChanged()));

	UMLEdgeElement *elementAsEdge = dynamic_cast<UMLEdgeElement*>(element);
	if (elementAsEdge != nullptr)
	{
		elementAsEdge->from()->m_outgoingEdges.removeOne(elementAsEdge);
		elementAsEdge->to()->m_incomingEdges.removeOne(elementAsEdge);
	}

	if (m_guiProxy)
	{
		m_guiProxy->notifyElementRemoved(element);

		if (elementAsEdge != nullptr)
		{
			m_guiProxy->notifyElementChanged(elementAsEdge->from());
			m_guiProxy->notifyElementChanged(elementAsEdge->to());
		}
	}

	m_elements.removeOne(element);
	delete element;
}

void UMLDiagram::deleteAllElements()
{
	// free elements in reverse order to avoid breaking references
	while (m_elements.size() != 0)
		deleteUMLElement(m_elements.last());
}

QString UMLDiagram::generateFreshName(const QString &prefix)
{
	int seqNum = 1;
	return QString("%1%2").arg(prefix).arg(seqNum);
}

void UMLDiagram::slotElementChanged()
{
	Core::UMLElement *element = qobject_cast<UMLElement*>(QObject::sender());

	if (m_guiProxy)
		m_guiProxy->notifyElementChanged(element);
}

}
