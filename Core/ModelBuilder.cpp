#include "Core/ModelBuilder.h"

#include "Core/Compiler/SemanticTreeGenerator.h"
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

	qDebug() << "Checking that no duplicate global names are present...";
	checkDuplicateGlobalNames();
	if (m_error)
		return;

	qDebug() << "Checking that control-flow and signal edges are used properly...";
	checkControlFlowEdges();
	checkSignalEdges();

	qDebug() << "Checking that types are sound...";
	registerTypes();
	if (m_error)
		return;

	qDebug() << "Registering global variables, signals and states...";
	registerGlobalVariables(); // this must be done before registering signals, see comment inside
	registerSignals();
	registerStates();
	if (m_error)
		return;

	qDebug() << "Success";
}

ModelBuilder::~ModelBuilder()
{
}

bool ModelBuilder::success() const
{
	return !m_error;
}

const Compiler::SemanticContext *ModelBuilder::semanticContext() const
{
	return &m_semanticContext;
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

const Compiler::SemanticTree::Type *ModelBuilder::resolveType(const DatatypeName *dt) const
{
	switch (dt->type())
	{
		case DatatypeName::Bool:
			return m_semanticContext.boolType();
		case DatatypeName::Other:
			return m_semanticContext.findOtherType(dt->datatypeName());
		case DatatypeName::Set:
		{
			const Compiler::SemanticTree::Type *inner = resolveType(dt->innerDatatype());
			return inner != nullptr ? m_semanticContext.findSetType(inner) : nullptr;
		}
		default:
			qFatal("This should never happen");
			return nullptr;
	}
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
			case UMLElementType::FlowFinalNode:
				definitionOrigins.insert(
					static_cast<const UMLNodeElement*>(elem)->nodeName(),
					"a/flow final node");
				break;
			case UMLElementType::ActivityFinalNode:
				definitionOrigins.insert(
					static_cast<const UMLNodeElement*>(elem)->nodeName(),
					"a/activity final node");
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

		const QString &nodeName = nodeElem->nodeName();

		QMap<QString, int> seenBranches;
		foreach (const UMLControlFlowEdge *edge, nodeElem->outgoingControlFlowEdges())
		{
			const QString &branchName = edge->branchName();
			if (!branchName.isEmpty() && seenBranches[branchName]++ == 1)
				emitError(
					QString("%1:[%2]").arg(nodeName).arg(branchName),
					"Branch name used more than once");
		}

		switch (nodeElem->type())
		{
			case UMLElementType::InitialNode:
				foreach (const UMLControlFlowEdge *edge, nodeElem->incomingControlFlowEdges())
				{
					if (edge->from()->type() != UMLElementType::ActivityFinalNode)
					{
						emitError(nodeName, "Initial nodes' incoming control-flow edges can only originate from activity final nodes");
						break;
					}
				}
				if (nodeElem->outgoingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Initial node without any outgoing control-flow edge");
				else if (nodeElem->outgoingControlFlowEdges().count() > 1)
					emitError(nodeName, "Initial nodes cannot have multiple outgoing control-flow edges");
				break;
			case UMLElementType::FlowFinalNode:
				if (nodeElem->incomingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Flow final node without any incoming control-flow edge");
				else if (nodeElem->incomingControlFlowEdges().count() > 1)
					emitError(nodeName, "Flow final nodes cannot have multiple incoming control-flow edges");
				if (nodeElem->outgoingControlFlowEdges().count() != 0)
					emitError(nodeName, "Flow final nodes cannot have outgoing control-flow edges");
				break;
			case UMLElementType::ActivityFinalNode:
				if (nodeElem->incomingControlFlowEdges().count() == 0)
					emitWarning(nodeName, "Activity final node without any incoming control-flow edge");
				else if (nodeElem->incomingControlFlowEdges().count() > 1)
					emitError(nodeName, "Activity final nodes cannot have multiple incoming control-flow edges");
				if (nodeElem->outgoingControlFlowEdges().count() > 1)
					emitError(nodeName, "Activity final nodes cannot have multiple outgoing control-flow edges");
				foreach (const UMLControlFlowEdge *edge, nodeElem->outgoingControlFlowEdges())
				{
					if (edge->to()->type() != UMLElementType::InitialNode)
					{
						emitError(nodeName, "Activity final nodes' outgoing control-flow edges can only point to initial nodes");
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

void ModelBuilder::registerTypes()
{
	// Register enumarations first. Classes and global variables will be handled later
	QMap<const UMLClass*, Core::Compiler::SemanticTree::ClassType*> umlClasses;
	foreach (const UMLElement *elem, m_doc->classDiagram()->elements())
	{
		switch (elem->type())
		{
			case UMLElementType::GlobalVariables:
				break;
			case UMLElementType::Enumeration:
				{
					const UMLEnumeration *enm = static_cast<const UMLEnumeration*>(elem);
					Core::Compiler::SemanticTree::EnumerationType *obj = m_semanticContext.registerEnumeration(enm->datatypeName());
					foreach (const QString &val, enm->values())
						obj->registerValue(val);
				}
				break;
			case UMLElementType::Class:
				{
					const UMLClass *cls = static_cast<const UMLClass*>(elem);
					umlClasses.insert(cls,
						m_semanticContext.registerClass(cls->datatypeName()));
				}
				break;
			default:
				qFatal("This should never happen");
				break;
		}
	}

	// Register classes' member variables
	foreach (const UMLClass *cls, umlClasses.keys())
	{
		Core::Compiler::SemanticTree::ClassType *obj = umlClasses[cls];

		QMap<QString, int> seenNames;
		foreach (const UMLClass::MemberVariable &var, cls->memberVariables())
		{
			if (seenNames[var.name]++ == 1)
				emitError(
					QString("%1:%2").arg(cls->datatypeName()).arg(var.name),
					"Variable defined more than once");
		}

		foreach (const UMLClass::MemberVariable &var, cls->memberVariables())
		{
			// Skip variables that are not unambiguously defined
			if (seenNames[var.name] > 1)
				continue;

			const Compiler::SemanticTree::Type *type = resolveType(&var.datatypeName);
			if (type != nullptr)
				obj->registerMemberVariable(var.name, type);
			else
				emitError(
					QString("%1:%2").arg(cls->datatypeName()).arg(var.name),
					QString("Cannot resolve type \"%1\"").arg(var.datatypeName.toString()));
		}
	}

	// Check that no type is defined in terms of itself
	foreach (Core::Compiler::SemanticTree::ClassType *obj, umlClasses)
	{
		QSet<const Core::Compiler::SemanticTree::Type*> referencedTypes;
		obj->fillReferencedTypes(referencedTypes);
		if (referencedTypes.contains(obj))
			emitError(
				QString("%1").arg(obj->datatypeName()),
				"Classes cannot be defined in terms of themselves (recursive datatypes are not supported)");
	}
}

void ModelBuilder::registerGlobalVariables()
{
	QList<const UMLGlobalVariables*> umlGlobalVars;
	foreach (const UMLElement *elem, m_doc->classDiagram()->elements())
	{
		switch (elem->type())
		{
			case UMLElementType::GlobalVariables:
				umlGlobalVars.append(static_cast<const UMLGlobalVariables*>(elem));
				break;
			case UMLElementType::Enumeration:
			case UMLElementType::Class:
				break;
			default:
				qFatal("This should never happen");
				break;
		}
	}

	// Resolve global variables' type and initial value.
	// This must be done before registering any global variable or signal,
	// so we can be sure that no variables are read in the initial value
	QMap<QString, const Compiler::SemanticTree::Type*> gvTypes;
	QMap<QString, const Compiler::SemanticTree::Expr*> gvInitValues;
	foreach (const UMLGlobalVariables *gv, umlGlobalVars)
	{
		foreach (const UMLGlobalVariables::GlobalVariable &var, gv->globalVariables())
		{
			const Compiler::SemanticTree::Type *type = resolveType(&var.datatypeName);
			if (type == nullptr)
			{
				emitError(
					QString("(global):%1").arg(var.name),
					QString("Cannot resolve type \"%1\"").arg(var.datatypeName.toString()));
			}
			else
			{
				Compiler::SemanticTreeGenerator stgen(var.initialValue, type, &m_semanticContext);
				if (stgen.success())
				{
					const Compiler::SemanticTree::Expr *initVal = stgen.takeResultExpr();
					gvTypes.insert(var.name, type);
					gvInitValues.insert(var.name, initVal);
				}
				else
				{
					emitError(
						QString("(init):%1:%2").arg(var.name).arg(stgen.errorLocation().toString()),
						stgen.errorMessage());
				}
			}
		}
	}

	foreach (const QString &gvName, gvTypes.keys())
		m_semanticContext.registerGlobalVariable(gvName, gvTypes[gvName], gvInitValues[gvName]);
}

void ModelBuilder::registerSignals()
{
	foreach (const UMLElement *elem, m_doc->activityDiagram()->elements())
	{
		const UMLSignalEdge *signalElem = dynamic_cast<const UMLSignalEdge*>(elem);
		if (signalElem == nullptr)
			continue;

		const QString &signalName = signalElem->signalName();
		const DatatypeName &datatype = signalElem->messageDatatypeName();
		const Core::Compiler::SemanticTree::Type *resolvedType;

		if (datatype.type() == DatatypeName::Invalid)
		{
			resolvedType = nullptr;
		}
		else
		{
			resolvedType = resolveType(&datatype);

			if (resolvedType == nullptr)
			{
				emitError(signalName,
					QString("Cannot resolve type \"%1\"").arg(datatype.toString()));
				continue;
			}
		}

		m_semanticContext.registerSignal(signalName, resolvedType);
	}
}

void ModelBuilder::registerStates()
{
	foreach (const UMLElement *elem, m_doc->activityDiagram()->elements())
	{
		const UMLNodeElement *signalElem = dynamic_cast<const UMLNodeElement*>(elem);
		if (signalElem == nullptr)
			continue;

		m_semanticContext.registerState(signalElem->nodeName());
	}
}

}
