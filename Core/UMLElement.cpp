#include "UMLElement.h"

#include <QDomDocument>

namespace Core
{

UMLElement::UMLElement(UMLElementType type)
: m_type(type), m_guiProxyPointer(nullptr)
{
}

UMLElement::~UMLElement()
{
}

UMLElementType UMLElement::type() const
{
	return m_type;
}

void UMLElement::setGuiProxyPointer(void *newPointer)
{
	m_guiProxyPointer = newPointer;
}

void *UMLElement::guiProxyPointer() const
{
	return m_guiProxyPointer;
}

void UMLElement::topoSort(QList<UMLElement*> &list, bool reverse)
{
	qSort(list.begin(), list.end(), [=](UMLElement *a, UMLElement *b)
	{
		return ((int)a->type() < (int)b->type()) != reverse;
	});
}

UMLNodeElement::UMLNodeElement(UMLElementType type)
: UMLElement(type)
{
}

UMLNodeElement::~UMLNodeElement()
{
	Q_ASSERT(m_incomingEdges.isEmpty());
	Q_ASSERT(m_outgoingEdges.isEmpty());
}

void UMLNodeElement::setNodeName(const QString &newName)
{
	m_nodeName = newName;
	emit changed();
}

const QString &UMLNodeElement::nodeName() const
{
	return m_nodeName;
}

const QList<UMLEdgeElement*> &UMLNodeElement::incomingEdges() const
{
	return m_incomingEdges;
}

const QList<UMLEdgeElement*> &UMLNodeElement::outgoingEdges() const
{
	return m_outgoingEdges;
}

void UMLNodeElement::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	target.setAttribute("nodeName", nodeName());
}

bool UMLNodeElement::loadFromXml(const QDomElement &source)
{
	setNodeName(source.attribute("nodeName"));
	return true;
}

UMLInitialNode::UMLInitialNode()
: UMLNodeElement(UMLElementType::InitialNode)
{
}

UMLFinalNode::UMLFinalNode()
: UMLNodeElement(UMLElementType::FinalNode)
{
}

UMLActionNode::UMLActionNode()
: UMLNodeElement(UMLElementType::ActionNode)
{
}

UMLDecisionMergeNode::UMLDecisionMergeNode()
: UMLNodeElement(UMLElementType::DecisionMergeNode)
{
}

UMLForkJoinNode::UMLForkJoinNode()
: UMLNodeElement(UMLElementType::ForkJoinNode)
{
}

UMLEdgeElement::UMLEdgeElement(UMLElementType type, UMLNodeElement *from, UMLNodeElement *to)
: UMLElement(type), m_from(from), m_to(to)
{
}

UMLNodeElement *UMLEdgeElement::from() const
{
	return m_from;
}

UMLNodeElement *UMLEdgeElement::to() const
{
	return m_to;
}

UMLControlFlowEdge::UMLControlFlowEdge(UMLNodeElement *from, UMLNodeElement *to)
: UMLEdgeElement(UMLElementType::ControlFlowEdge, from, to)
{
}

void UMLControlFlowEdge::setBranchName(const QString &newName)
{
	m_branchName = newName;
	emit changed();
}

const QString &UMLControlFlowEdge::branchName() const
{
	return m_branchName;
}

void UMLControlFlowEdge::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	target.setAttribute("branchName", branchName());
}

bool UMLControlFlowEdge::loadFromXml(const QDomElement &source)
{
	setBranchName(source.attribute("branchName"));
	return true;
}

UMLSignalEdge::UMLSignalEdge(UMLNodeElement *from, UMLNodeElement *to)
: UMLEdgeElement(UMLElementType::SignalEdge, from, to)
{
}

void UMLSignalEdge::setSignalName(const QString &newName)
{
	m_signalName = newName;
	emit changed();
}

const QString &UMLSignalEdge::signalName() const
{
	return m_signalName;
}

void UMLSignalEdge::setMessageDatatypeName(const DatatypeName &newDatatypeName)
{
	m_datatypeName = newDatatypeName;
}

const DatatypeName &UMLSignalEdge::messageDatatypeName() const
{
	return m_datatypeName;
}

void UMLSignalEdge::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	target.setAttribute("signalName", signalName());

	if (m_datatypeName.type() != Core::DatatypeName::Invalid)
	{
		QDomElement messageDatatypeElem = doc.createElement("message-datatype");
		target.appendChild(messageDatatypeElem);
		m_datatypeName.storeToXml(messageDatatypeElem, doc);
	}
}

bool UMLSignalEdge::loadFromXml(const QDomElement &source)
{
	setSignalName(source.attribute("signalName"));

	QDomElement messageDatatypeElem = source.firstChildElement("message-datatype");
	if (!messageDatatypeElem.isNull())
		setMessageDatatypeName(Core::DatatypeName(messageDatatypeElem));

	return true;
}

UMLDatatypeElement::UMLDatatypeElement(UMLElementType type)
: UMLElement(type)
{
}

void UMLDatatypeElement::setDatatypeName(const QString &newName)
{
	m_datatypeName = newName;
	emit changed();
}

const QString &UMLDatatypeElement::datatypeName() const
{
	return m_datatypeName;
}

void UMLDatatypeElement::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	target.setAttribute("datatypeName", datatypeName());
}

bool UMLDatatypeElement::loadFromXml(const QDomElement &source)
{
	setDatatypeName(source.attribute("datatypeName"));
	return true;
}

UMLClass::MemberVariable::MemberVariable(const QString &name, const DatatypeName &datatypeName)
: name(name), datatypeName(datatypeName)
{
}

UMLClass::UMLClass()
: UMLDatatypeElement(UMLElementType::Class)
{
}

void UMLClass::setMemberVariables(const QList<MemberVariable> &vars)
{
	m_memberVariables = vars;
	emit changed();
}

const QList<UMLClass::MemberVariable> &UMLClass::memberVariables() const
{
	return m_memberVariables;
}

void UMLClass::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	UMLDatatypeElement::storeToXml(target, doc);

	QDomElement memVarsElem = doc.createElement("member-variables");
	target.appendChild(memVarsElem);

	foreach (const MemberVariable &var, memberVariables())
	{
		QDomElement varElem = doc.createElement("variable");
		QDomElement nameElem = doc.createElement("name");
		QDomElement datatypeElem = doc.createElement("datatype");
		memVarsElem.appendChild(varElem);
		varElem.appendChild(nameElem);
		varElem.appendChild(datatypeElem);
		nameElem.appendChild(doc.createTextNode(var.name));
		var.datatypeName.storeToXml(datatypeElem, doc);
	}
}

bool UMLClass::loadFromXml(const QDomElement &source)
{
	if (!UMLDatatypeElement::loadFromXml(source))
		return false;

	QDomElement valuesElem = source.firstChildElement("member-variables");
	QList<MemberVariable> values;
	for (QDomElement varElem = valuesElem.firstChildElement();
		!varElem.isNull();
		varElem = varElem.nextSiblingElement())
	{
		QDomElement nameElem = varElem.firstChildElement("name");
		QDomElement datatypeElem = varElem.firstChildElement("datatype");
		values.append(MemberVariable(
			nameElem.text(), DatatypeName(datatypeElem)));
	}

	setMemberVariables(values);
	return true;
}

UMLEnumeration::UMLEnumeration()
: UMLDatatypeElement(UMLElementType::Enumeration)
{
}

void UMLEnumeration::setValues(const QStringList &values)
{
	m_values = values;
	emit changed();
}

const QStringList &UMLEnumeration::values() const
{
	return m_values;
}

void UMLEnumeration::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	UMLDatatypeElement::storeToXml(target, doc);

	QDomElement valuesElem = doc.createElement("values");
	target.appendChild(valuesElem);

	foreach (const QString &val, values())
	{
		QDomElement valueElem = doc.createElement("value");
		valuesElem.appendChild(valueElem);
		valueElem.appendChild(doc.createTextNode(val));
	}
}

bool UMLEnumeration::loadFromXml(const QDomElement &source)
{
	if (!UMLDatatypeElement::loadFromXml(source))
		return false;

	QDomElement valuesElem = source.firstChildElement("values");
	QStringList values;
	for (QDomElement valueElem = valuesElem.firstChildElement();
		!valueElem.isNull();
		valueElem = valueElem.nextSiblingElement())
	{
		values.append(valueElem.text());
	}

	setValues(values);
	return true;
}

UMLGlobalVariables::GlobalVariable::GlobalVariable(const QString &name,
	const DatatypeName &datatypeName, bool isPersistent, const QString &initialValue)
: name(name), datatypeName(datatypeName), isPersistent(isPersistent), initialValue(initialValue)
{
}

UMLGlobalVariables::UMLGlobalVariables()
: UMLElement(UMLElementType::GlobalVariables)
{
}

void UMLGlobalVariables::setGlobalVariables(const QList<GlobalVariable> &vars)
{
	m_globalVariables = vars;
	emit changed();
}

const QList<UMLGlobalVariables::GlobalVariable> &UMLGlobalVariables::globalVariables() const
{
	return m_globalVariables;
}

void UMLGlobalVariables::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	foreach (const GlobalVariable &var, globalVariables())
	{
		QDomElement varElem = doc.createElement("variable");
		QDomElement nameElem = doc.createElement("name");
		QDomElement datatypeElem = doc.createElement("datatype");
		QDomElement initValElem = doc.createElement("initial-value");
		target.appendChild(varElem);
		varElem.appendChild(nameElem);
		varElem.appendChild(datatypeElem);
		varElem.appendChild(initValElem);
		varElem.setAttribute("persistent", var.isPersistent ? "true" : "false");
		nameElem.appendChild(doc.createTextNode(var.name));
		var.datatypeName.storeToXml(datatypeElem, doc);
		initValElem.appendChild(doc.createTextNode(var.initialValue));
	}
}

bool UMLGlobalVariables::loadFromXml(const QDomElement &source)
{
	QList<GlobalVariable> values;
	for (QDomElement varElem = source.firstChildElement();
		!varElem.isNull();
		varElem = varElem.nextSiblingElement())
	{
		QDomElement nameElem = varElem.firstChildElement("name");
		QDomElement datatypeElem = varElem.firstChildElement("datatype");
		QDomElement initValElem = varElem.firstChildElement("initial-value");

		values.append(Core::UMLGlobalVariables::GlobalVariable(nameElem.text(),
			Core::DatatypeName(datatypeElem),
			varElem.attribute("persistent") == "true", initValElem.text()));
	}

	setGlobalVariables(values);
	return true;
}

}
