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

QString Compiler::compileInitialNode(const UMLInitialNode *node)
{
	QString result = QString("\n// InitialNode \"%1\"\n").arg(node->nodeName());
	result += QString("%1 : [0..2] init 1;\n").arg(node->nodeName());

	// InitialNodes can only have 0 or 1 outgoing edge
	const UMLControlFlowEdge *edge = (node->outgoingControlFlowEdges().count() == 0) ?
		nullptr : node->outgoingControlFlowEdges().first();

	if (edge != nullptr)
		result += QString("[] %1>0 -> 1.0 : (%1'=0) & (%2'=%2+1);\n").arg(node->nodeName()).arg(edge->to()->nodeName());

	return result;
}

QString Compiler::compileFlowFinalNode(const UMLFlowFinalNode *node)
{
	QString result = QString("\n// FlowFinalNode \"%1\"\n").arg(node->nodeName());
	result += QString("%1 : [0..2] init 0;\n").arg(node->nodeName());

	// FlowFinalNodes cannot have any outgoing edge
	result += QString("[] %1>0 -> 1.0 : (%1'=0);\n").arg(node->nodeName());
	return result;
}

QString Compiler::compileActivityFinalNode(const UMLActivityFinalNode *node)
{
	QString result = QString("\n// ActivityFinalNode \"%1\"\n").arg(node->nodeName());
	result += QString("%1 : [0..2] init 0;\n").arg(node->nodeName());

	// ActivityFinalNodes can only have 0 or 1 outgoing edge, pointing to the InitialNode
	const UMLInitialNode *edge = (node->outgoingControlFlowEdges().count() == 0) ?
		nullptr : static_cast<const UMLInitialNode*>(node->outgoingControlFlowEdges().first()->to());

	if (edge != nullptr)
		result += QString("[] %1>0 -> 1.0 : TODO-RESTART;\n").arg(node->nodeName());

	return result;
}

QString Compiler::compileActionNode(const UMLActionNode *node, const SemanticTree::Stmt *script,
	const QString &nextNode, ErrorList *out_errorList)
{
	QString result = QString("\n// ActionNode \"%1\"\n").arg(node->nodeName());

	if (script != nullptr)
		result += QString("// Script: %1\n").arg(script->toString());
	else
		result += "// No script\n";

	if (!nextNode.isEmpty())
		result += QString("// Next is %1\n").arg(nextNode);
	else
		result += "// No next\n";

	if (script != nullptr)
	{
		char *rawResult = (char*)hsCompileScriptedAction(script->haskellHandle());
		result += rawResult;
		free(rawResult);
	}

	//*out_errorList << Error(ErrorType::Warning, "Warning! Compilation is not implemented yet"); // TODO
	return result;
}

QString Compiler::compileDecisionMergeNode(const UMLDecisionMergeNode *node, const SemanticTree::Stmt *script,
	const QString &nextNode, ErrorList *out_errorList)
{
	QString result = QString("\n// DecisionMergeNode \"%1\"\n").arg(node->nodeName());

	if (script != nullptr)
		result += QString("// Script: %1\n").arg(script->toString());
	else
		result += "// No script\n";

	if (!nextNode.isEmpty())
		result += QString("// Default next is %1\n").arg(nextNode);
	else
		result += "// No default next\n";

	if (script != nullptr)
	{
		char *rawResult = (char*)hsCompileScriptedAction(script->haskellHandle());
		result += rawResult;
		free(rawResult);
	}

	//*out_errorList << Error(ErrorType::Warning, "Warning! Compilation is not implemented yet"); // TODO
	return result;
}

QString Compiler::compileForkJoinNode(const UMLForkJoinNode *node)
{
	QString result = QString("\n// ForkJoinNode \"%1\"\n").arg(node->nodeName());

	// DecisionMergeNodes can have any number of incoming and outgoing edges
	const int incomingCount = node->incomingControlFlowEdges().count();
	QStringList outgoingList = QStringList() << QString("(%1'=0)").arg(node->nodeName());

	foreach (const UMLControlFlowEdge *elem, node->outgoingControlFlowEdges())
		outgoingList.append(QString("(%1'=%1+1)").arg(elem->to()->nodeName()));

	result += QString("%1 : [0..%2] init 0;\n").arg(node->nodeName()).arg(incomingCount + 1);
	result += QString("[] %1>%2 -> 1.0 : %3;\n").arg(node->nodeName()).arg(qMax(incomingCount - 1, 0)).arg(outgoingList.join(" & "));

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
