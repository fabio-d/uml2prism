#ifndef CORE_UMLELEMENT_H
#define CORE_UMLELEMENT_H

#include <QList>
#include <QObject>
#include <QString>

class QDomDocument;
class QDomElement;

namespace Core
{

class UMLEdgeElement;

// These types are listed in a order such that objects that can be referenced by
// other objects always come first
enum class UMLElementType
{
	// Activity diagrams
	InitialNode,
	FinalNode,
	ActionNode,
	DecisionMergeNode,
	ForkJoinNode,
	ControlFlowEdge,
	SignalEdge,

	// Class diagrams
	Class,
	Enumeration
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

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	private:
		QString m_nodeName;
		QList<UMLEdgeElement*> m_incomingEdges;
		QList<UMLEdgeElement*> m_outgoingEdges;
};

class UMLInitialNode : public UMLNodeElement
{
	public:
		UMLInitialNode();
};

class UMLFinalNode : public UMLNodeElement
{
	public:
		UMLFinalNode();
};

class UMLActionNode : public UMLNodeElement
{
	public:
		UMLActionNode();
};

class UMLDecisionMergeNode : public UMLNodeElement
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

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	private:
		UMLNodeElement *m_from, *m_to;
};

class UMLControlFlowEdge : public UMLEdgeElement
{
	public:
		UMLControlFlowEdge(UMLNodeElement *from, UMLNodeElement *to);
};

class UMLSignalEdge : public UMLEdgeElement
{
	public:
		UMLSignalEdge(UMLNodeElement *from, UMLNodeElement *to);
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
		UMLClass();
};

class UMLEnumeration : public UMLDatatypeElement
{
	public:
		UMLEnumeration();
};

}

#endif // CORE_UMLELEMENT_H
