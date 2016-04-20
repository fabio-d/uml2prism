#ifndef CORE_UMLELEMENT_H
#define CORE_UMLELEMENT_H

#include <QList>

namespace Core
{

// These types are listed in a order such that objects that can be referenced by
// other objects always come first
enum class UMLElementType
{
	InitialNode
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

class UMLInitialNode : public UMLElement
{
	public:
		UMLInitialNode();
};

}

#endif // CORE_UMLELEMENT_H
