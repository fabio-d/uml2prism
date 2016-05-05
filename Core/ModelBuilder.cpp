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
	checkDuplicateNames();
	if (m_error)
		return;

	qDebug() << "Success";
}

ModelBuilder::~ModelBuilder()
{
}

void ModelBuilder::emitError(const QString &location, const QString &description)
{
	qDebug() << "ERROR:" << location << ":" << description;
	m_error = true;
}

void ModelBuilder::checkDuplicateNames()
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

}
