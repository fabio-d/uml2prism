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

#ifndef CORE_UMLELEMENT_H
#define CORE_UMLELEMENT_H

#include "Core/DatatypeName.h"

#include <QObject>
#include <QPair>
#include <QStringList>

class QDomDocument;
class QDomElement;

namespace Core
{

class UMLControlFlowEdge;
class UMLEdgeElement;
class UMLSignalEdge;

// These types are listed in a order such that objects that can be referenced by
// other objects always come first:
//  - ControlFlowEdge may contain references to nodes
//  - SignalEdge may contain references to nodes
enum class UMLElementType
{
	// Activity diagrams
	InitialNode,
	FlowFinalNode,
	ActivityFinalNode,
	ActionNode,
	DecisionMergeNode,
	ForkJoinNode,
	ControlFlowEdge,
	SignalEdge,

	// Class diagrams
	Class,
	Enumeration,
	GlobalVariables
};

class UMLElement : public QObject
{
	Q_OBJECT

	public:
		virtual ~UMLElement();
		UMLElementType type() const;

		// The GuiProxy can use this field to store its own data
		void setGuiProxyPointer(void *newPointer);
		void *guiProxyPointer() const;

		// Store/load from XML element
		virtual void storeToXml(QDomElement &target, QDomDocument &doc) const = 0;
		virtual bool loadFromXml(const QDomElement &source) = 0;

		// Sort list of elements according to their type (see
		// comment in the definition of UMLElementType)
		static void topoSort(QList<UMLElement*> &list, bool reverse = false);

	signals:
		void changed();

	protected:
		explicit UMLElement(UMLElementType type);

	private:
		const UMLElementType m_type;
		void *m_guiProxyPointer;
};

class UMLNodeElement : public UMLElement
{
	friend class UMLDiagram;

	public:
		explicit UMLNodeElement(UMLElementType type);
		~UMLNodeElement();

		void setNodeName(const QString &newName);
		const QString &nodeName() const;

		const QList<UMLEdgeElement*> &incomingEdges() const;
		const QList<UMLEdgeElement*> &outgoingEdges() const;

		QList<UMLControlFlowEdge*> incomingControlFlowEdges() const;
		QList<UMLControlFlowEdge*> outgoingControlFlowEdges() const;

		QList<UMLSignalEdge*> incomingSignalEdges() const;
		QList<UMLSignalEdge*> outgoingSignalEdges() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	private:
		static QList<UMLControlFlowEdge*> filterUMLControlFlowEdges(const QList<UMLEdgeElement*> &list);
		static QList<UMLSignalEdge*> filterUMLSignalEdgeEdges(const QList<UMLEdgeElement*> &list);

		QString m_nodeName;
		QList<UMLEdgeElement*> m_incomingEdges;
		QList<UMLEdgeElement*> m_outgoingEdges;
};

class UMLInitialNode : public UMLNodeElement
{
	public:
		UMLInitialNode();
};

class UMLFlowFinalNode : public UMLNodeElement
{
	public:
		UMLFlowFinalNode();
};

class UMLActivityFinalNode : public UMLNodeElement
{
	public:
		UMLActivityFinalNode();
};

class UMLScriptedNodeElement : public UMLNodeElement
{
	public:
		void setCustomScript(const QString &newScript);
		void unsetCustomScript(); // i.e. automatically generate script

		bool hasCustomScript() const;
		const QString &customScript() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	protected:
		UMLScriptedNodeElement(UMLElementType type);

	private:
		bool m_hasCustomScript;
		QString m_customScript;
};

class UMLActionNode : public UMLScriptedNodeElement
{
	public:
		UMLActionNode();
};

class UMLDecisionMergeNode : public UMLScriptedNodeElement
{
	public:
		UMLDecisionMergeNode();
};

class UMLForkJoinNode : public UMLNodeElement
{
	public:
		UMLForkJoinNode();
};

class UMLEdgeElement : public UMLElement
{
	public:
		UMLEdgeElement(UMLElementType type, UMLNodeElement *from, UMLNodeElement *to);

		UMLNodeElement *from() const;
		UMLNodeElement *to() const;

	private:
		UMLNodeElement *m_from, *m_to;
};

class UMLControlFlowEdge : public UMLEdgeElement
{
	public:
		UMLControlFlowEdge(UMLNodeElement *from, UMLNodeElement *to);

		void setBranchName(const QString &newName);
		const QString &branchName() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	private:
		QString m_branchName;
};

class UMLSignalEdge : public UMLEdgeElement
{
	public:
		UMLSignalEdge(UMLNodeElement *from, UMLNodeElement *to);

		void setSignalName(const QString &newName);
		const QString &signalName() const;

		// Note: type=DatatypeName::Invalid means no attached message
		void setMessageDatatypeName(const DatatypeName &newDatatypeName);
		const DatatypeName &messageDatatypeName() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	private:
		QString m_signalName;
		DatatypeName m_datatypeName;
};

class UMLDatatypeElement : public UMLElement
{
	public:
		explicit UMLDatatypeElement(UMLElementType type);

		void setDatatypeName(const QString &newName);
		const QString &datatypeName() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	private:
		QString m_datatypeName;
};

class UMLClass : public UMLDatatypeElement
{
	public:
		struct MemberVariable
		{
			MemberVariable() = default;
			MemberVariable(const QString &name, const DatatypeName &datatypeName);

			QString name;
			DatatypeName datatypeName;
		};

		UMLClass();

		void setMemberVariables(const QList<MemberVariable> &vars);
		const QList<MemberVariable> &memberVariables() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	private:
		QList<MemberVariable> m_memberVariables;
};

class UMLEnumeration : public UMLDatatypeElement
{
	public:
		UMLEnumeration();

		void setValues(const QStringList &values);
		const QStringList &values() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	private:
		QStringList m_values;
};

class UMLGlobalVariables : public UMLElement
{
	public:
		struct GlobalVariable
		{
			GlobalVariable() = default;
			GlobalVariable(const QString &name, const DatatypeName &datatypeName, bool isPersistent, const QString &initialValue);

			QString name;
			DatatypeName datatypeName;
			bool isPersistent;
			QString initialValue;
		};

		UMLGlobalVariables();

		void setGlobalVariables(const QList<GlobalVariable> &vars);
		const QList<GlobalVariable> &globalVariables() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	private:
		QList<GlobalVariable> m_globalVariables;
};

}

#endif // CORE_UMLELEMENT_H
