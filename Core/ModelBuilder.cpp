/*
 * Copyright (C) 2016 Fabio D'Urso
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "Core/ModelBuilder.h"

#include "Core/Compiler/Compiler.h"
#include "Core/Compiler/SemanticTreeGenerator.h"
#include "Core/Document.h"
#include "Core/PredicateList.h"
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

ModelBuilder::ModelBuilder(const Document *doc, bool debugOutput)
: m_doc(doc), m_started(false), m_error(false), m_modelOutput("mdp\n\n"),
  m_verboseDebugEnabled(debugOutput)
{
}

ModelBuilder::~ModelBuilder()
{
	qDeleteAll(m_globalVarsInitValue);
}

bool ModelBuilder::run()
{
	Q_ASSERT(m_started == false);

	if (m_verboseDebugEnabled)
		qDebug() << "ModelBuilder started";
	m_started =  true;

	if (m_verboseDebugEnabled)
		qDebug() << "Checking that no duplicate global names are present...";
	checkDuplicateGlobalNames();
	if (m_error)
		return false;

	if (m_verboseDebugEnabled)
		qDebug() << "Checking that control-flow and signal edges are used properly...";
	checkControlFlowEdges();
	checkSignalEdges();

	if (m_verboseDebugEnabled)
		qDebug() << "Checking that types are sound...";
	registerTypes();
	if (m_error)
		return false;

	if (m_verboseDebugEnabled)
		qDebug() << "Registering global variables, signals...";
	registerGlobalVariables(); // this must be done before registering signals, see comment inside
	registerSignals();
	if (m_error)
		return false;

	if (m_verboseDebugEnabled)
		qDebug() << "Compiling global variable and signal declarations...";
	m_modelOutput += compileVariableDecls();
	m_modelOutput += compileSignalDecls();

	m_modelOutput += "module MyModule\n";

	if (m_verboseDebugEnabled)
		qDebug() << "Compiling and registering states...";
	m_modelOutput += compileStates();
	registerStates(); // states must be registered after compiling scripts because scripts are not meant to be aware of them

	m_modelOutput += "\nendmodule\n";

	if (m_error)
		return false;

	if (m_verboseDebugEnabled)
		qDebug() << "Compiling and registering labels...";
	m_propertiesOutput += compileLabels();
	registerLabels(); // labels must be registered after compiling labels because labels are not meant to be aware of themselves

	if (m_error)
		return false;

	if (m_verboseDebugEnabled)
		qDebug() << "Compiling properties...";
	if (!m_propertiesOutput.isEmpty())
		m_propertiesOutput += "\n";
	m_propertiesOutput += compileProperties();

	if (m_error)
		return false;

	if (m_verboseDebugEnabled)
		qDebug() << "Success";
	return true;
}

bool ModelBuilder::success() const
{
	Q_ASSERT(m_started == true);
	return !m_error;
}

const Compiler::SemanticContext *ModelBuilder::semanticContext() const
{
	Q_ASSERT(m_started == true && m_error == false);
	return &m_semanticContext;
}

const QString &ModelBuilder::modelOutput() const
{
	Q_ASSERT(m_started == true && m_error == false);
	return m_modelOutput;
}

QString ModelBuilder::pctlPropertiesOutput() const
{
	Q_ASSERT(m_started == true && m_error == false);
	return QString(m_propertiesOutput).replace("$forall$", "P>=1").replace("$exists$", "!P<=0");
}

QString ModelBuilder::ctlPropertiesOutput() const
{
	Q_ASSERT(m_started == true && m_error == false);
	return QString(m_propertiesOutput).replace("$forall$", "A").replace("$exists$", "E");
}

void ModelBuilder::emitWarning(const QString &location, const QString &description)
{
	if (m_verboseDebugEnabled)
		qDebug() << "WARNING:" << location << ":" << description;
	emit warning(location, description);
}

void ModelBuilder::emitError(const QString &location, const QString &description)
{
	if (m_verboseDebugEnabled)
		qDebug() << "ERROR:" << location << ":" << description;
	m_error = true;
	emit error(location, description);
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

	foreach (const Predicate &p, m_doc->labels()->predicates())
		definitionOrigins.insert(p.name(), "l");

	foreach (const Predicate &p, m_doc->properties()->predicates())
		definitionOrigins.insert(p.name(), "p");

	foreach (const QString &name, definitionOrigins.uniqueKeys())
	{
		if (definitionOrigins.count(name) == 1)
			continue;

		const QList<QString> occurrences = definitionOrigins.values(name);
		QMap<QString, int> activityDiagramOccurrenceCountByType, classDiagramOccurrenceCountByType;
		int activityDiagramOccurrenceTotalCount = 0;
		int classDiagramOccurrenceTotalCount = 0;
		int labelListOccurrenceCount = 0;
		int propertyListOccurrenceCount = 0;

		foreach (const QString &occ, occurrences)
		{
			if (occ.startsWith("a/"))
			{
				activityDiagramOccurrenceCountByType[occ.mid(2)]++;
				activityDiagramOccurrenceTotalCount++;
			}
			else if (occ.startsWith("c/"))
			{
				classDiagramOccurrenceCountByType[occ.mid(2)]++;
				classDiagramOccurrenceTotalCount++;
			}
			else if (occ == "l")
			{
				labelListOccurrenceCount++;
			}
			else if (occ == "p")
			{
				propertyListOccurrenceCount++;
			}
			else
			{
				qFatal("This should never happen");
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

		QStringList locTexts;
		if (!activityDiagramOccurrenceDescriptions.isEmpty())
			locTexts.append(QString("in Activity Diagram (%1)").arg(naturalTextJoin(activityDiagramOccurrenceDescriptions)));
		if (!classDiagramOccurrenceDescriptions.isEmpty())
			locTexts.append(QString("in Class Diagram (%1)").arg(naturalTextJoin(classDiagramOccurrenceDescriptions)));
		if (labelListOccurrenceCount == 1)
			locTexts.append(QString("in Label list"));
		else if (labelListOccurrenceCount > 1)
			locTexts.append(QString("in Label list (more than once)"));
		if (propertyListOccurrenceCount == 1)
			locTexts.append(QString("in Property list"));
		else if (propertyListOccurrenceCount > 1)
			locTexts.append(QString("in Property list (more than once)"));

		emitError(name, "Name defined multiple times " + naturalTextJoin(locTexts));
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

const Compiler::SemanticTree::Stmt *ModelBuilder::parseCustomScript(const UMLScriptedNodeElement *elem)
{
	Q_ASSERT(elem->hasCustomScript() == true);

	QMap<QString, QString> labelMap;
	QStringList signalList;
	QString defaultBranchTarget; // only valid if there is one and only one outgoing edge
	bool defaultBranchTargetIsValid = false;

	foreach (const Core::UMLEdgeElement *edge, elem->outgoingEdges())
	{
		const Core::UMLControlFlowEdge *branchEdge =
			dynamic_cast<const Core::UMLControlFlowEdge*>(edge);
		const Core::UMLSignalEdge *signalEdge =
			dynamic_cast<const Core::UMLSignalEdge*>(edge);
		Q_ASSERT(!branchEdge != !signalEdge);

		if (branchEdge != nullptr)
		{
			if (defaultBranchTargetIsValid) // more than one edge
				defaultBranchTargetIsValid = false;

			if (defaultBranchTarget.isEmpty()) // first edge so far
			{
				defaultBranchTarget = branchEdge->to()->nodeName();
				defaultBranchTargetIsValid = true;
			}

			if (branchEdge->branchName().isEmpty())
				continue;
			labelMap.insert(branchEdge->branchName(), branchEdge->to()->nodeName());
		}
		else // signalEdge != nullptr
		{
			signalList.append(signalEdge->signalName());
		}
	}

	if (defaultBranchTargetIsValid == false)
		defaultBranchTarget.clear();

	Compiler::SemanticTreeGenerator stgen(
		elem->customScript(),
		&m_semanticContext,
		signalList,
		labelMap,
		defaultBranchTarget);

	if (stgen.success())
		return stgen.takeResultStmt();

	emitError(QString("%1:%2").arg(elem->nodeName()).arg(stgen.errorLocation().toString()), stgen.errorMessage());
	return nullptr;
}

const Compiler::SemanticTree::Stmt *ModelBuilder::generateDefaultScript(const UMLNodeElement *elem)
{
	QStringList signalsWithoutMessage;
	bool error = false;

	foreach (const Core::UMLSignalEdge *signalEdge, elem->outgoingSignalEdges())
	{
		const QString &signalName = signalEdge->signalName();
		const DatatypeName &datatype = signalEdge->messageDatatypeName();

		if (datatype.type() == DatatypeName::Invalid)
		{
			// This is a signal without attachment, we can
			// auto-generate the script!
			signalsWithoutMessage.append(signalName);
		}
		else
		{
			emitError(elem->nodeName(),
				QString("Signal \"%1\" has attachment of type %2: a custom script is required")
					.arg(signalName)
					.arg(datatype.datatypeName()));
		}
	}

	if (error)
		return nullptr;

	QList<const Compiler::SemanticTree::Stmt*> stmts;
	foreach (const QString &signalName, signalsWithoutMessage)
	{
		Compiler::SemanticTree::IdntGlobal *dest = new Compiler::SemanticTree::IdntGlobal(signalName, m_semanticContext.boolType());
		Compiler::SemanticTree::ExprBoolLiteral *val = new Compiler::SemanticTree::ExprBoolLiteral(true);
		stmts.append(new Compiler::SemanticTree::StmtAssignment(dest, val));
	}

	return new Compiler::SemanticTree::StmtCompound(stmts);
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
				Compiler::SemanticTreeGenerator stgen(var.initialValue, &m_semanticContext, type, false);
				if (stgen.success())
				{
					const Compiler::SemanticTree::Expr *initVal = stgen.takeResultExpr();
					gvTypes.insert(var.name, type);
					m_globalVarsInitValue.insert(var.name, initVal);

					if (var.isPersistent)
						m_persistentVariables.append(var.name);
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
		m_semanticContext.registerGlobalVariable(gvName, gvTypes[gvName]);
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
		const UMLNodeElement *nodeElem = dynamic_cast<const UMLNodeElement*>(elem);
		if (nodeElem == nullptr)
			continue;

		m_semanticContext.registerState(nodeElem->nodeName());
	}
}

void ModelBuilder::registerLabels()
{
	foreach (const Predicate &p, m_doc->labels()->predicates())
		m_semanticContext.registerLabel(p.name());
}

QString ModelBuilder::compileVariableDecls()
{
	QString result;

	Compiler::Compiler comp(&m_semanticContext);
	foreach (const QString &varName, m_globalVarsInitValue.keys())
	{
		const Compiler::SemanticTree::Type *type =
			m_semanticContext.findGlobalVariableOrSignalWithMessage(varName);
		const Compiler::SemanticTree::Expr *initVal =
			m_globalVarsInitValue[varName];
		const bool isPersistent = m_persistentVariables.contains(varName);

		result += comp.compileVariableDeclaration(varName, type, initVal, isPersistent) + "\n";
	}

	return result;
}

QString ModelBuilder::compileSignalDecls()
{
	QString result;

	Compiler::Compiler comp(&m_semanticContext);
	foreach (const UMLElement *elem, m_doc->activityDiagram()->elements())
	{
		const UMLSignalEdge *signalElem = dynamic_cast<const UMLSignalEdge*>(elem);
		if (signalElem == nullptr)
			continue;

		const QString &signalName = signalElem->signalName();
		const DatatypeName &datatype = signalElem->messageDatatypeName();
		const Core::Compiler::SemanticTree::Type *resolvedType;

		if (datatype.type() == DatatypeName::Invalid)
			resolvedType = nullptr;
		else
			resolvedType = resolveType(&datatype);

		result += comp.compileSignalDeclaration(signalName, resolvedType) + "\n";
	}

	return result;
}

QString ModelBuilder::compileStates()
{
	QString declLines;
	QString result;

	foreach (const UMLElement *elem, m_doc->activityDiagram()->elements())
	{
		const UMLNodeElement *nodeElem = dynamic_cast<const UMLNodeElement*>(elem);
		if (nodeElem != nullptr)
		{
			Compiler::Compiler comp(&m_semanticContext);
			Compiler::Compiler::ErrorList errList;

			int maxValue = 1;
			QString declLine;

			switch (elem->type())
			{
				case UMLElementType::InitialNode:
				{
					result += comp.compileInitialNode(static_cast<const UMLInitialNode*>(nodeElem), &declLine);
					break;
				}
				case UMLElementType::FlowFinalNode:
				{
					result += comp.compileFlowFinalNode(static_cast<const UMLFlowFinalNode*>(nodeElem), &declLine);
					break;
				}
				case UMLElementType::ActivityFinalNode:
				{
					QStringList allStates;
					QMap<QString, const Compiler::SemanticTree::Expr *> restartVarValues;
					QMap<QString, const Compiler::SemanticTree::Type *> restartSignalTypes;

					foreach (const UMLElement *elem2, m_doc->activityDiagram()->elements())
					{
						const UMLNodeElement *nodeElem2 = dynamic_cast<const UMLNodeElement*>(elem2);
						if (nodeElem2 != nullptr)
							allStates.append(nodeElem2->nodeName());
					}

					foreach (const QString &varName, m_globalVarsInitValue.keys())
					{
						const Compiler::SemanticTree::Expr *initVal =
							m_globalVarsInitValue[varName];
						if (m_persistentVariables.contains(varName) == false)
							restartVarValues.insert(varName, initVal);
					}

					foreach (const UMLElement *elem, m_doc->activityDiagram()->elements())
					{
						const UMLSignalEdge *signalElem = dynamic_cast<const UMLSignalEdge*>(elem);
						if (signalElem != nullptr)
						{
							const DatatypeName &datatype = signalElem->messageDatatypeName();
							const Core::Compiler::SemanticTree::Type *resolvedType;

							if (datatype.type() == DatatypeName::Invalid)
								resolvedType = m_semanticContext.boolType();
							else
								resolvedType = resolveType(&datatype);

							restartSignalTypes.insert(signalElem->signalName(), resolvedType);
						}
					}

					result += comp.compileActivityFinalNode(static_cast<const UMLActivityFinalNode*>(nodeElem),
						&declLine, allStates, restartVarValues, restartSignalTypes);
					break;
				}
				case UMLElementType::ActionNode:
				{
					const UMLActionNode *actionNode = static_cast<const UMLActionNode*>(nodeElem);
					QScopedPointer<const Compiler::SemanticTree::Stmt> script;

					if (actionNode->hasCustomScript())
					{
						script.reset(parseCustomScript(actionNode));
						if (script.isNull())
							continue; // parse error
					}
					else
					{
						script.reset(generateDefaultScript(actionNode));
						if (script.isNull())
							continue; // default script cannot be generated
					}

					// ActionNodes can only have 0 or 1 outgoing edge
					const QString nextNode = (actionNode->outgoingControlFlowEdges().count() == 0) ?
						QString() : actionNode->outgoingControlFlowEdges().first()->to()->nodeName();

					// Append branch to nextNode
					if (!nextNode.isEmpty())
					{
						script.reset(new Compiler::SemanticTree::StmtCompound(
							QList<const Compiler::SemanticTree::Stmt*>()
								<< script.take()
								<< new Compiler::SemanticTree::StmtBranch(nextNode)
							));
					}

					result += comp.compileActionNode(actionNode, &declLine, script.data(),
						!nextNode.isEmpty(), &errList);
					break;
				}
				case UMLElementType::DecisionMergeNode:
				{
					const UMLDecisionMergeNode *decisionMergeNode = static_cast<const UMLDecisionMergeNode*>(nodeElem);
					QScopedPointer<const Compiler::SemanticTree::Stmt> script;

					if (decisionMergeNode->hasCustomScript())
					{
						script.reset(parseCustomScript(decisionMergeNode));
						if (script.isNull())
							continue; // parse error
					}
					else
					{
						script.reset(generateDefaultScript(decisionMergeNode));
						if (script.isNull())
							continue; // default script cannot be generated
					}

					// DecisionMergeNode can have any number of outgoing edges. If
					// there is only one, set it as default if no branch is provided
					// by the custom script (e.g. a merge node)
					const QString nextNode = (decisionMergeNode->outgoingControlFlowEdges().count() != 1) ?
						QString() : decisionMergeNode->outgoingControlFlowEdges().first()->to()->nodeName();

					// Append branch to nextNode
					if (!nextNode.isEmpty())
					{
						script.reset(new Compiler::SemanticTree::StmtCompound(
							QList<const Compiler::SemanticTree::Stmt*>()
								<< script.take()
								<< new Compiler::SemanticTree::StmtBranch(nextNode)
							));
					}

					result += comp.compileDecisionMergeNode(decisionMergeNode, &declLine, script.data(),
						decisionMergeNode->outgoingControlFlowEdges().count() > 0, &errList);
					break;
				}
				case UMLElementType::ForkJoinNode:
				{
					result += comp.compileForkJoinNode(static_cast<const UMLForkJoinNode*>(nodeElem), &declLine, &maxValue);
					break;
				}
				default:
				{
					qFatal("This should never happen");
					break;
				}
			}

			declLines += declLine;
			m_maxValueByState.insert(nodeElem->nodeName(), maxValue);

			foreach (const Compiler::Compiler::Error &error, errList)
			{
				if (error.first == Compiler::Compiler::ErrorType::Error)
					emitError(nodeElem->nodeName(), error.second);
				else
					emitWarning(nodeElem->nodeName(), error.second);
			}
		}
	}

	// Fill "${TARGET_NODE_NAME}" placeholders
	foreach (const QString &stateName, m_maxValueByState.keys())
		result.replace(QString("$max%1$").arg(stateName), QString::number(m_maxValueByState[stateName]));

	return declLines + result;
}

QString ModelBuilder::compileLabels()
{
	Compiler::Compiler comp(&m_semanticContext);
	QString result;

	foreach (const Predicate &p, m_doc->labels()->predicates())
	{
		Core::Compiler::SemanticTreeGenerator stgen(
			p.expression(),
			&m_semanticContext,
			m_semanticContext.boolType(),
			false);

		if (!stgen.success())
		{
			emitError(QString("%1:%2").arg(p.name()).arg(stgen.errorLocation().toString()), stgen.errorMessage());
			continue;
		}

		const Core::Compiler::SemanticTree::Expr *semTree = stgen.takeResultExpr();
		result += comp.compileLabel(p.name(), semTree);
		delete semTree;
	}

	return result;
}

QString ModelBuilder::compileProperties()
{
	Compiler::Compiler comp(&m_semanticContext);
	QString result;

	foreach (const Predicate &p, m_doc->properties()->predicates())
	{
		Core::Compiler::SemanticTreeGenerator stgen(
			p.expression(),
			&m_semanticContext,
			m_semanticContext.boolType(),
			true);

		if (!stgen.success())
		{
			emitError(QString("%1:%2").arg(p.name()).arg(stgen.errorLocation().toString()), stgen.errorMessage());
			continue;
		}

		if (!result.isEmpty())
			result += "\n";

		const Core::Compiler::SemanticTree::Expr *semTree = stgen.takeResultExpr();
		result += comp.compileProperty(p.name(), semTree);
		delete semTree;
	}

	return result;
}

}
