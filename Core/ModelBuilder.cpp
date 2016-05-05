#include "Core/ModelBuilder.h"

#include "Core/Document.h"
#include "Core/UMLDiagram.h"
#include "Core/UMLElement.h"

#include <QDebug>
#include <QString>

// ["a", "b", "c"] -> "a, b and c"
static QString naturalTextJoin(const QStringList &list)
{
	if (list.isEmpty())
		return QString();

	QStringList localCopy = list;
	const QString lastElem = localCopy.takeLast();

	if (localCopy.isEmpty())
		return lastElem;

	return QString("%1 and %2").arg(localCopy.join(", ")).arg(lastElem);
}

namespace Core
{

ModelBuilder::ModelBuilder(const Document *doc)
: m_doc(doc), m_error(false)
{
	qDebug() << "ModelBuilder started";

	qDebug() << "Checking that no duplicate names are present...";
	checkDuplicateGlobalNames();
	if (m_error)
		return;

	// TODO: check types

	qDebug() << "Checking that control-flow and signal edges are used properly...";
	checkControlFlowEdges();
	checkSignalEdges();
	if (m_error)
		return;

	qDebug() << "Success";
}

ModelBuilder::~ModelBuilder()
{
}

void ModelBuilder::emitWarning(const QString &location, const QString &description)
{
	qDebug() << "WARNING:" << location << ":" << description;
}

void ModelBuilder::emitError(const QString &location, const QString &description)
{
	qDebug() << "ERROR:" << location << ":" << description;
	m_error = true;
}

void ModelBuilder::checkDuplicateGlobalNames()
{
	// name -> description of the origin of the name
	QMultiMap<QString, QString> definitionOrigins;
	foreach (const UMLElement *elem, m_doc->activityDiagram()->elements())
	{
		switch (elem->type())
		{
			case UMLElementType::InitialNode:
				definitionOrigins.insert(
					static_cast<const UMLNodeElement*>(elem)->nodeName(),
					"a/initial node");
				break;
			case UMLElementType::FinalNode:
				definitionOrigins.insert(
					static_cast<const UMLNodeElement*>(elem)->nodeName(),
					"a/final node");
				break;
			case UMLElementType::ActionNode:
				definitionOrigins.insert(
					static_cast<const UMLNodeElement*>(elem)->nodeName(),
					"a/action node");
				break;
			case UMLElementType::DecisionMergeNode:
				definitionOrigins.insert(
					static_cast<const UMLNodeElement*>(elem)->nodeName(),
					"a/decision/merge node");
				break;
			case UMLElementType::ForkJoinNode:
				definitionOrigins.insert(
					static_cast<const UMLNodeElement*>(elem)->nodeName(),
					"a/fork/join node");
				break;
			case UMLElementType::ControlFlowEdge:
				// branch names are not global names
				break;
			case UMLElementType::SignalEdge:
				definitionOrigins.insert(
					static_cast<const UMLSignalEdge*>(elem)->signalName(),
					"a/signal name");
				break;
			default:
				qFatal("This should never happen");
				break;
		}
	}
	foreach (const UMLElement *elem, m_doc->classDiagram()->elements())
	{
		switch (elem->type())
		{
			case UMLElementType::GlobalVariables:
				foreach (const UMLGlobalVariables::GlobalVariable &var,
					static_cast<const UMLGlobalVariables*>(elem)->globalVariables())
				{
					definitionOrigins.insert(
						var.name,
						"c/global variable");
				}
				break;
			case UMLElementType::Enumeration:
				{
					const UMLEnumeration *enm = static_cast<const UMLEnumeration*>(elem);
					definitionOrigins.insert(
						enm->datatypeName(),
						"c/enumeration type");

					foreach (const QString &val, enm->values())
					{
						definitionOrigins.insert(
							val,
							QString("c/%1 value").arg(enm->datatypeName()));
					}
				}
				break;
			case UMLElementType::Class:
				definitionOrigins.insert(
					static_cast<const UMLClass*>(elem)->datatypeName(),
					"c/class type");
				break;
			default:
				qFatal("This should never happen");
				break;
		}
	}

	foreach (const QString &name, definitionOrigins.uniqueKeys())
	{
		if (definitionOrigins.count(name) == 1)
			continue;

		const QList<QString> occurrences = definitionOrigins.values(name);
		QMap<QString, int> activityDiagramOccurrenceCountByType, classDiagramOccurrenceCountByType;
		int activityDiagramOccurrenceTotalCount = 0;
		int classDiagramOccurrenceTotalCount = 0;

		foreach (const QString &occ, occurrences)
		{
			// occurrences must be either from the activity or class diagram
			Q_ASSERT(occ.startsWith("a/") || occ.startsWith("c/"));

			if (occ.startsWith("a/"))
			{
				activityDiagramOccurrenceCountByType[occ.mid(2)]++;
				activityDiagramOccurrenceTotalCount++;
			}
			else
			{
				classDiagramOccurrenceCountByType[occ.mid(2)]++;
				classDiagramOccurrenceTotalCount++;
			}
		}

		QStringList activityDiagramOccurrenceDescriptions;
		if (activityDiagramOccurrenceTotalCount == 1)
		{
			activityDiagramOccurrenceDescriptions.append(activityDiagramOccurrenceCountByType.begin().key());
		}
		else
		{
			foreach (const QString &occ, activityDiagramOccurrenceCountByType.keys())
			{
				const int count = activityDiagramOccurrenceCountByType[occ];
				if (count == 1)
					activityDiagramOccurrenceDescriptions.append(QString("1 %1").arg(occ));
				else
					activityDiagramOccurrenceDescriptions.append(QString("%1 %2s").arg(count).arg(occ));
			}
		}

		QStringList classDiagramOccurrenceDescriptions;
		if (classDiagramOccurrenceTotalCount == 1)
		{
			classDiagramOccurrenceDescriptions.append(classDiagramOccurrenceCountByType.begin().key());
		}
		else
		{
			foreach (const QString &occ, classDiagramOccurrenceCountByType.keys())
			{
				const int count = classDiagramOccurrenceCountByType[occ];
				if (count == 1)
					classDiagramOccurrenceDescriptions.append(QString("1 %1").arg(occ));
				else
					classDiagramOccurrenceDescriptions.append(QString("%1 %2s").arg(count).arg(occ));
			}
		}

		QString locText;
		if (!activityDiagramOccurrenceDescriptions.isEmpty())
		{
			locText = QString("in Activity Diagram (%1)").arg(naturalTextJoin(activityDiagramOccurrenceDescriptions));
		}
		if (!classDiagramOccurrenceDescriptions.isEmpty())
		{
			if (!activityDiagramOccurrenceDescriptions.isEmpty())
				locText += " and ";
			locText += QString("in Class Diagram (%1)").arg(naturalTextJoin(classDiagramOccurrenceDescriptions));
		}

		emitError(name, "Name defined multiple times " + locText);
	}
}

void ModelBuilder::checkControlFlowEdges()
{
	foreach (const UMLElement *elem, m_doc->activityDiagram()->elements())
	{
		const UMLNodeElement *nodeElem = dynamic_cast<const UMLNodeElement*>(elem);
		if (nodeElem == nullptr)
			continue;

		const QString nodeName = nodeElem->nodeName();

		switch (nodeElem->type())
		{
			case UMLElementType::InitialNode:
				foreach (const UMLControlFlowEdge *edge, nodeElem->incomingControlFlowEdges())
				{
					if (edge->from()->type() != UMLElementType::FinalNode)
					{
						emitError(nodeName, "Initial nodes' incoming control-flow edges can only originate from final nodes");
						break;
					}
				}
				if (nodeElem->outgoingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Initial node without any outgoing control-flow edge");
				else if (nodeElem->outgoingControlFlowEdges().count() > 1)
					emitError(nodeName, "Initial nodes cannot have multiple outgoing control-flow edges");
				break;
			case UMLElementType::FinalNode:
				if (nodeElem->incomingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Final node without any incoming control-flow edge");
				else if (nodeElem->incomingControlFlowEdges().count() > 1)
					emitError(nodeName, "Final nodes cannot have multiple incoming control-flow edges");
				if (nodeElem->outgoingControlFlowEdges().count() > 1)
					emitError(nodeName, "Final nodes cannot have multiple outgoing control-flow edges");
				foreach (const UMLControlFlowEdge *edge, nodeElem->outgoingControlFlowEdges())
				{
					if (edge->to()->type() != UMLElementType::InitialNode)
					{
						emitError(nodeName, "Final nodes' outgoing control-flow edges can only point to initial nodes");
						break;
					}
				}
				break;
			case UMLElementType::ActionNode:
				if (nodeElem->incomingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Action node without any incoming control-flow edge");
				else if (nodeElem->incomingControlFlowEdges().count() > 1)
					emitError(nodeName, "Action nodes cannot have multiple incoming control-flow edges");
				if (nodeElem->outgoingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Action node without any outgoing control-flow edge");
				else if (nodeElem->outgoingControlFlowEdges().count() > 1)
					emitError(nodeName, "Action nodes cannot have multiple outgoing control-flow edges");
				break;
			case UMLElementType::ForkJoinNode:
				if (nodeElem->incomingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Fork/Join node without any incoming control-flow edge");
				if (nodeElem->outgoingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Fork/Join node without any outgoing control-flow edge");
				if (nodeElem->incomingControlFlowEdges().count() == 1
					&& nodeElem->outgoingControlFlowEdges().count() == 1)
				{
					emitWarning(nodeName, "Fork/Join node with only one incoming and only one outgoing control-flow edge is useless");
				}
				break;
			case UMLElementType::DecisionMergeNode:
				if (nodeElem->incomingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Decision/Merge node without any incoming control-flow edge");
				if (nodeElem->outgoingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Decision/Merge node without any outgoing control-flow edge");
				if (nodeElem->incomingControlFlowEdges().count() == 1
					&& nodeElem->outgoingControlFlowEdges().count() == 1)
				{
					emitWarning(nodeName, "Decision/Merge node with only one incoming and only one outgoing control-flow edge is useless");
				}
				break;
			default:
				qFatal("This should never happen");
				break;
		}

		if (nodeElem->type() != UMLElementType::DecisionMergeNode
			|| nodeElem->outgoingControlFlowEdges().count() == 1) // or merge node
		{
			foreach (const UMLControlFlowEdge *edge, nodeElem->outgoingControlFlowEdges())
			{
				if (edge->branchName().isEmpty() == false)
				{
					emitWarning(nodeName, "Non-decision node's outgoing control-flow edge's label is useless");
					break;
				}
			}
		}
		else
		{
			foreach (const UMLControlFlowEdge *edge, nodeElem->outgoingControlFlowEdges())
			{
				if (edge->branchName().isEmpty() == true)
				{
					emitWarning(nodeName, "Decision node's outgoing control-flow edge has no label");
					break;
				}
			}
		}
	}
}

void ModelBuilder::checkSignalEdges()
{
	foreach (const UMLElement *elem, m_doc->activityDiagram()->elements())
	{
		const UMLSignalEdge *signalElem = dynamic_cast<const UMLSignalEdge*>(elem);
		if (signalElem == nullptr)
			continue;

		switch (signalElem->from()->type())
		{
			case UMLElementType::ActionNode:
			case UMLElementType::DecisionMergeNode:
				break;
			default:
				emitError(signalElem->signalName(),
					"Signals' origin nodes can only be action, decision or merge nodes");
				break;
		}

		switch (signalElem->to()->type())
		{
			case UMLElementType::ActionNode:
			case UMLElementType::DecisionMergeNode:
				break;
			default:
				emitError(signalElem->signalName(),
					"Signals' destination nodes can only be action, decision or merge nodes");
				break;
		}
	}
}

}
