#include "Core/UMLDiagram.h"

#include "Core/GuiProxy.h"
#include "Core/UMLElement.h"

#include <QDebug>
#include <QDomDocument>

namespace Core
{

UMLDiagram::UMLDiagram(Document *doc, Type type)
: m_doc(doc), m_type(type), m_guiProxy(nullptr)
{
}

UMLDiagram::~UMLDiagram()
{
	deleteAllElements();
}

Document *UMLDiagram::document() const
{
	return m_doc;
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
	UMLElement::topoSort(m_elements);
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

QString UMLDiagram::generateFreshName(const QString &prefix) const
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

void UMLDiagram::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	foreach (UMLElement *elem, m_elements)
	{
		UMLEdgeElement *edge = nullptr;

		QDomElement rootElem = doc.createElement("element");
		target.appendChild(rootElem);

		switch (elem->type())
		{
			case UMLElementType::InitialNode:
				rootElem.setAttribute("type", "Initial");
				rootElem.setAttribute("id", m_elements.indexOf(elem));
				break;
			case UMLElementType::FinalNode:
				rootElem.setAttribute("type", "Final");
				rootElem.setAttribute("id", m_elements.indexOf(elem));
				break;
			case UMLElementType::ActionNode:
				rootElem.setAttribute("type", "Action");
				rootElem.setAttribute("id", m_elements.indexOf(elem));
				break;
			case UMLElementType::DecisionMergeNode:
				rootElem.setAttribute("type", "DecisionMerge");
				rootElem.setAttribute("id", m_elements.indexOf(elem));
				break;
			case UMLElementType::ForkJoinNode:
				rootElem.setAttribute("type", "ForkJoin");
				rootElem.setAttribute("id", m_elements.indexOf(elem));
				break;
			case UMLElementType::ControlFlowEdge:
				rootElem.setAttribute("type", "ControlFlowEdge");
				edge = static_cast<UMLEdgeElement*>(elem);
				break;
			case UMLElementType::SignalEdge:
				rootElem.setAttribute("type", "SignalEdge");
				edge = static_cast<UMLEdgeElement*>(elem);
				break;
			case UMLElementType::Class:
				rootElem.setAttribute("type", "Class");
				break;
			case UMLElementType::Enumeration:
				rootElem.setAttribute("type", "Enumeration");
				break;
		}

		// If this is an edge, store its endpoints too
		if (edge != nullptr)
		{
			rootElem.setAttribute("from", m_elements.indexOf(edge->from()));
			rootElem.setAttribute("to", m_elements.indexOf(edge->to()));
		}

		QDomElement coreElem = doc.createElement("core");
		rootElem.appendChild(coreElem);
		elem->storeToXml(coreElem, doc);

		QDomElement guiElem = doc.createElement("gui");
		rootElem.appendChild(guiElem);
		if (m_guiProxy)
			m_guiProxy->storeGuiDataToXml(elem, guiElem, doc);
	}
}

bool UMLDiagram::loadFromXml(const QDomElement &source)
{
	deleteAllElements();

	QMap<int, UMLNodeElement*> idToNodeMap;

	for (QDomElement rootElem = source.firstChildElement();
		!rootElem.isNull();
		rootElem = rootElem.nextSiblingElement())
	{
		const QString type = rootElem.attribute("type");
		const int id = rootElem.attribute("id").toInt();
		const int from = rootElem.attribute("from").toInt();
		const int to = rootElem.attribute("to").toInt();

		Core::UMLElement *elem;
		if (type == "Initial")
		{
			elem = *idToNodeMap.insert(id, new Core::UMLInitialNode());
		}
		else if (type == "Final")
		{
			elem = *idToNodeMap.insert(id, new Core::UMLFinalNode());
		}
		else if (type == "Action")
		{
			elem = *idToNodeMap.insert(id, new Core::UMLActionNode());
		}
		else if (type == "DecisionMerge")
		{
			elem = *idToNodeMap.insert(id, new Core::UMLDecisionMergeNode());
		}
		else if (type == "ForkJoin")
		{
			elem = *idToNodeMap.insert(id, new Core::UMLForkJoinNode());
		}
		else if (type == "ControlFlowEdge")
		{
			elem = new Core::UMLControlFlowEdge(
				idToNodeMap.value(from), idToNodeMap.value(to));
		}
		else if (type == "SignalEdge")
		{
			elem = new Core::UMLSignalEdge(
				idToNodeMap.value(from), idToNodeMap.value(to));
		}
		else if (type == "Class")
		{
			elem = new Core::UMLClass();
		}
		else if (type == "Enumeration")
		{
			elem = new Core::UMLEnumeration();
		}
		else
		{
			qDebug() << "Unrecognized element type" << type;
			return false;
		}

		QDomElement coreElem = rootElem.firstChildElement("core");
		QDomElement guiElem = rootElem.firstChildElement("gui");

		if (!elem->loadFromXml(coreElem))
		{
			qDebug() << "Failed to parse core data";
			return false;
		}

		if (m_guiProxy && !m_guiProxy->loadGuiDataFromXml(elem, guiElem))
		{
			qDebug() << "Failed to parse gui data";
			return false;
		}

		addUMLElement(elem);
	}

	return true;
}

}
