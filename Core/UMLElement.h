#ifndef CORE_UMLELEMENT_H
#define CORE_UMLELEMENT_H

#include <QList>
#include <QObject>
#include <QString>

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
	DecisionNode,
	MergeNode,
	ForkNode,
	JoinNode,
	ControlFlowEdge,

	// Class diagrams
	Class
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
	friend class UMLDocument;

	public:
		UMLNodeElement(UMLElementType type, const QString &nodeName);
		~UMLNodeElement();

		void setNodeName(const QString &newName);
		const QString &nodeName() const;

		const QList<UMLEdgeElement*> &incomingEdges() const;
		const QList<UMLEdgeElement*> &outgoingEdges() const;

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

class UMLDecisionNode : public UMLNodeElement
{
	public:
		UMLDecisionNode();
};

class UMLMergeNode : public UMLNodeElement
{
	public:
		UMLMergeNode();
};

class UMLForkNode : public UMLNodeElement
{
	public:
		UMLForkNode();
};

class UMLJoinNode : public UMLNodeElement
{
	public:
		UMLJoinNode();
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
};

class UMLClass : public UMLElement
{
	public:
		UMLClass();

		void setClassName(const QString &newName);
		const QString &className() const;

	private:
		QString m_className;
};

}

#endif // CORE_UMLELEMENT_H
