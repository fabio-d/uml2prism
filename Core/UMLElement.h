#ifndef CORE_UMLELEMENT_H
#define CORE_UMLELEMENT_H

#include <QList>
#include <QString>

namespace Core
{

// These types are listed in a order such that objects that can be referenced by
// other objects always come first
enum class UMLElementType
{
	InitialNode,
	DecisionNode,
	MergeNode
};

class UMLElement
{
	public:
		virtual ~UMLElement();
		UMLElementType type() const;

		// The GuiProxy can use this field to store its own data
		void setGuiProxyPointer(void *newPointer);
		void *guiProxyPointer() const;

		// Sort list of elements according to their type (see
		// comment in the definition of UMLElementType)
		static void topoSort(QList<UMLElement*> &list, bool reverse = false);

	protected:
		explicit UMLElement(UMLElementType type);

	private:
		const UMLElementType m_type;
		void *m_guiProxyPointer;
};

class UMLNodeElement : public UMLElement
{
	public:
		UMLNodeElement(UMLElementType type, const QString &nodeName);

		void setNodeName(const QString &newName);
		const QString &nodeName() const;

	private:
		QString m_nodeName;
};

class UMLInitialNode : public UMLNodeElement
{
	public:
		UMLInitialNode();
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

}

#endif // CORE_UMLELEMENT_H
