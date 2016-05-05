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

const QList<UMLElement*> &UMLDiagram::elements() const
{
	return m_elements;
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

QStringList UMLDiagram::listNames(bool datatypesOnly) const
{
	QStringList result;

	foreach (const UMLElement *elem, m_elements)
	{
		switch (elem->type())
		{
			case UMLElementType::InitialNode:
			case UMLElementType::FinalNode:
			case UMLElementType::ActionNode:
			case UMLElementType::DecisionMergeNode:
			case UMLElementType::ForkJoinNode:
				if (!datatypesOnly)
					result.append(static_cast<const UMLNodeElement*>(elem)->nodeName());
				break;
			case UMLElementType::ControlFlowEdge:
				// branch names are not global names
				break;
			case UMLElementType::SignalEdge:
				if (!datatypesOnly)
					result.append(static_cast<const UMLSignalEdge*>(elem)->signalName());
				break;
			case UMLElementType::GlobalVariables:
				if (!datatypesOnly)
				{
					foreach (const UMLGlobalVariables::GlobalVariable &var,
						static_cast<const UMLGlobalVariables*>(elem)->globalVariables())
					{
						result.append(var.name);
					}
				}
				break;
			case UMLElementType::Enumeration:
				if (!datatypesOnly)
				{
					foreach (const QString &val,
						static_cast<const UMLEnumeration*>(elem)->values())
					{
						result.append(val);
					}
				}
				/* fallthrough */
			case UMLElementType::Class:
				result.append(static_cast<const UMLDatatypeElement*>(elem)->datatypeName());
				break;
		}
	}

	return result;
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
			case UMLElementType::GlobalVariables:
				rootElem.setAttribute("type", "GlobalVariables");
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
		else if (type == "GlobalVariables")
		{
			elem = new Core::UMLGlobalVariables();
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
