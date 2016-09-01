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

#include "Core/Compiler/Compiler.h"

#include "Core/Compiler/SemanticContext.h"
#include "Core/UMLElement.h"

#include <QDebug>

namespace Core
{
namespace Compiler
{

Compiler::Compiler(const SemanticContext *context)
: m_context(context)
{
}

QString Compiler::escapeString(const QString &str)
{
	const QByteArray n = str.toLatin1();
	char *rawResult = (char*)hsEscapeString((void*)n.constData());
	const QString result = rawResult;
	free(rawResult);
	return result;
}

QString Compiler::compileVariableDeclaration(const QString &name,
	const SemanticTree::Type *type, const SemanticTree::Expr *initialValue,
	bool isPersistent)
{
	const QByteArray n = name.toLatin1();
	char *rawResult = (char*)hsCompileVariableDeclaration(
		(void*)n.constData(),
		initialValue->haskellHandle());
	const QString result = QString("// %1 %2 : %3\n%4")
		.arg(isPersistent ? "Persistent variable" : "Variable")
		.arg(name)
		.arg(type->datatypeName())
		.arg(rawResult);
	free(rawResult);

	return result;
}

QString Compiler::compileSignalDeclaration(const QString &name,
	const SemanticTree::Type *type)
{
	QString result = QString("// Signal \"%1\"").arg(name);

	if (type == nullptr)
	{
		type = m_context->boolType();
		result += "\n";
	}
	else
	{
		result += QString(" : %1\n").arg(type->datatypeName());
	}

	const QByteArray n = name.toLatin1();
	char *rawResult = (char*)hsCompileSignalDeclaration(
		(void*)n.constData(),
		type->haskellHandle());
	result += rawResult;
	free(rawResult);

	return result;
}

QString Compiler::compileInitialNode(const UMLInitialNode *node, QString *out_declLine)
{
	QString result = QString("\n// InitialNode \"%1\"\n").arg(node->nodeName());
	*out_declLine = QString("%1 : [0..1] init 1;\n").arg(escapeString(node->nodeName()));

	// InitialNodes can only have 0 or 1 outgoing edge
	const UMLControlFlowEdge *edge = (node->outgoingControlFlowEdges().count() == 0) ?
		nullptr : node->outgoingControlFlowEdges().first();

	if (edge != nullptr)
	{
		result += QString("[] (%1>0) & (%2<$max%3$) -> 1.0 : (%1'=0) & (%2'=%2+1);\n")
			.arg(escapeString(node->nodeName()))
			.arg(escapeString(edge->to()->nodeName()))
			.arg(edge->to()->nodeName());
	}

	return result;
}

QString Compiler::compileFlowFinalNode(const UMLFlowFinalNode *node, QString *out_declLine)
{
	QString result = QString("\n// FlowFinalNode \"%1\"\n").arg(node->nodeName());
	*out_declLine = QString("%1 : [0..1] init 0;\n").arg(escapeString(node->nodeName()));

	// FlowFinalNodes cannot have any outgoing edge
	result += QString("[] (%1>0) -> 1.0 : (%1'=0);\n").arg(escapeString(node->nodeName()));
	return result;
}

QString Compiler::compileActivityFinalNode(const UMLActivityFinalNode *node, QString *out_declLine,
	const QStringList &allStates,
	const QMap<QString, const SemanticTree::Expr *> &restartVarValues,
	const QMap<QString, const SemanticTree::Type *> &restartSignalTypes)
{
	QString result = QString("\n// ActivityFinalNode \"%1\"\n").arg(node->nodeName());
	*out_declLine = QString("%1 : [0..1] init 0;\n").arg(escapeString(node->nodeName()));

	// ActivityFinalNodes can only have 0 or 1 outgoing edge, pointing to the InitialNode
	const UMLInitialNode *restartNode = (node->outgoingControlFlowEdges().count() == 0) ?
		nullptr : static_cast<const UMLInitialNode*>(node->outgoingControlFlowEdges().first()->to());

	QStringList actions;

	// if an edge is present, it points back to the initial node and it means
	// we have to restart
	if (restartNode != nullptr)
	{
		actions.append(QString("(%1'=1)").arg(escapeString(restartNode->nodeName())));

		foreach (const QString &stateName, allStates)
		{
			if (stateName != restartNode->nodeName())
				actions.append(QString("(%1'=0)").arg(escapeString(stateName)));
		}

		foreach (const QString &varName, restartVarValues.keys())
		{
			const QByteArray n = varName.toLatin1();
			char *rawResult = (char*)hsCompileSimpleAssignment(
				(void*)n.constData(),
				restartVarValues[varName]->haskellHandle());
			if (rawResult[0] != '\0')
				actions.append(rawResult);
			free(rawResult);
		}

		foreach (const QString &varName, restartSignalTypes.keys())
		{
			const QByteArray n = varName.toLatin1();
			char *rawResult = (char*)hsCompileNilAssignment(
				(void*)n.constData(),
				restartSignalTypes[varName]->haskellHandle());
			if (rawResult[0] != '\0')
				actions.append(rawResult);
			free(rawResult);
		}
	}
	else
	{
		foreach (const QString &stateName, allStates)
			actions.append(QString("(%1'=0)").arg(escapeString(stateName)));
	}

	result += QString("[] %1>0 -> 1.0 : %2;\n").arg(node->nodeName()).arg(actions.join(" & "));

	return result;
}

QString Compiler::compileScriptedNode(const UMLScriptedNodeElement *node,
	const SemanticTree::Stmt *script, bool branchEnabled, ErrorList *out_errorList)
{
	const QByteArray n = node->nodeName().toLatin1();
	char *rawResult = (char*)hsCompileScriptedAction(
		(void*)n.constData(),
		script->haskellHandle(),
		branchEnabled);
	QString hsResult = rawResult;
	free(rawResult);

	if (hsResult.startsWith("G/")) // success
	{
		QString replacement;

		// Add waits for incoming signals
		foreach (const UMLSignalEdge *signalElem, node->incomingSignalEdges())
		{
			const QString &signalName = signalElem->signalName();
			const Core::Compiler::SemanticTree::Type *type =
				m_context->findGlobalVariableOrSignalWithMessage(signalName) ?: m_context->boolType();

			const QByteArray n2 = signalName.toLatin1();
			char *rawResult2 = (char*)hsCompileNotNilCheck(
				(void*)n2.constData(),
				type->haskellHandle());
			replacement += QString(" & %1").arg(rawResult2);
			free(rawResult2);
		}

		return hsResult.mid(2).replace("$signalWaitConditions$", replacement);
	}
	else if (hsResult.startsWith("E/"))
		*out_errorList << Error(ErrorType::Error, hsResult.mid(2));
	else
		qFatal("This should never happen");

	return QString();
}

QString Compiler::compileActionNode(const UMLActionNode *node, QString *out_declLine,
	const SemanticTree::Stmt *script, bool branchEnabled, ErrorList *out_errorList)
{
	QString result = QString("\n// ActionNode \"%1\"\n").arg(node->nodeName());
	*out_declLine = QString("%1 : [0..1] init 0;\n").arg(escapeString(node->nodeName()));
	result += compileScriptedNode(node, script, branchEnabled, out_errorList);
	return result;
}

QString Compiler::compileDecisionMergeNode(const UMLDecisionMergeNode *node, QString *out_declLine,
	const SemanticTree::Stmt *script, bool branchEnabled, ErrorList *out_errorList)
{
	QString result = QString("\n// DecisionMergeNode \"%1\"\n").arg(node->nodeName());
	*out_declLine = QString("%1 : [0..1] init 0;\n").arg(escapeString(node->nodeName()));
	result += compileScriptedNode(node, script, branchEnabled, out_errorList);
	return result;
}

QString Compiler::compileForkJoinNode(const UMLForkJoinNode *node, QString *out_declLine, int *out_maxValue)
{
	QString result = QString("\n// ForkJoinNode \"%1\"\n").arg(node->nodeName());

	// DecisionMergeNodes can have any number of incoming and outgoing edges
	const int incomingCount = node->incomingControlFlowEdges().count();
	QStringList guardList = QStringList() << QString("(%1>%2)").arg(escapeString(node->nodeName())).arg(qMax(incomingCount - 1, 0));
	QStringList actionList = QStringList() << QString("(%1'=0)").arg(escapeString(node->nodeName()));

	foreach (const UMLControlFlowEdge *elem, node->outgoingControlFlowEdges())
	{
		guardList.append(QString("(%1<$max%2$)").arg(escapeString(elem->to()->nodeName())).arg(elem->to()->nodeName()));
		actionList.append(QString("(%1'=%1+1)").arg(escapeString(elem->to()->nodeName())));
	}

	*out_maxValue = qMax(incomingCount, 1);
	*out_declLine = QString("%1 : [0..%2] init 0;\n")
		.arg(escapeString(node->nodeName()))
		.arg(*out_maxValue);
	result += QString("[] %1 -> 1.0 : %2;\n")
		.arg(guardList.join(" & "))
		.arg(actionList.join(" & "));

	return result;
}

QString Compiler::compileLabel(const QString &name, const SemanticTree::Expr *pred)
{
	const QByteArray n = name.toLatin1();
	QString result;
	char *rawResult = (char*)hsCompileLabel((void*)n.constData(), pred->haskellHandle());
	result = rawResult;
	free(rawResult);
	return result;
}

QString Compiler::compileProperty(const QString &name, const SemanticTree::Expr *pred)
{
	char *rawResult = (char*)hsCompileProperty(pred->haskellHandle());
	QString result = QString("// Property \"%1\"\n%2\n").arg(name).arg(rawResult);
	free(rawResult);
	return result;
}

}
}
