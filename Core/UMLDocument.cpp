#include "Core/UMLDocument.h"

#include "Core/GuiProxy.h"
#include "Core/UMLElement.h"

namespace Core
{

UMLDocument::UMLDocument()
: m_guiProxy(nullptr)
{
}

UMLDocument::~UMLDocument()
{
	deleteAllElements();
}

void UMLDocument::setGuiProxy(GuiProxy *guiProxy)
{
	m_guiProxy = guiProxy;
}

GuiProxy *UMLDocument::guiProxy() const
{
	return m_guiProxy;
}

void UMLDocument::addUMLElement(UMLElement *element)
{
	Q_ASSERT(m_elements.contains(element) == false);

	if (m_guiProxy)
		m_guiProxy->notifyElementAdded(element);

	connect(element, SIGNAL(changed()), this, SLOT(slotElementChanged()));
	m_elements.append(element);
}

bool UMLDocument::deleteUMLElement(UMLElement *element)
{
	Q_ASSERT(m_elements.contains(element) == true);
	disconnect(element, SIGNAL(changed()), this, SLOT(slotElementChanged()));

	if (m_guiProxy)
		m_guiProxy->notifyElementRemoved(element);

	m_elements.removeOne(element);
	delete element;

	return true; // TODO: fail if element is referenced by some other element
}

void UMLDocument::deleteAllElements()
{
	// free elements in reverse order to avoid breaking references
	while (m_elements.size() != 0)
	{
		bool success = deleteUMLElement(m_elements.last());
		Q_ASSERT(success);
	}
}

void UMLDocument::slotElementChanged()
{
	Core::UMLElement *element = qobject_cast<UMLElement*>(QObject::sender());

	if (m_guiProxy)
		m_guiProxy->notifyElementChanged(element);
}

}
