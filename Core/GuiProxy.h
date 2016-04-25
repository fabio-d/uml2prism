#ifndef CORE_GUIPROXY_H
#define CORE_GUIPROXY_H

class QDomDocument;
class QDomElement;

namespace Core
{

class UMLElement;

class GuiProxy
{
	public:
		virtual void notifyElementAdded(UMLElement *element) = 0;
		virtual void notifyElementChanged(UMLElement *element) = 0;
		virtual void notifyElementRemoved(UMLElement *element) = 0;

		virtual void storeGuiDataToXml(UMLElement *element, QDomElement &target, QDomDocument &doc) const = 0;
		virtual bool loadGuiDataFromXml(UMLElement *element, const QDomElement &source) = 0;
};

}

#endif // CORE_GUIPROXY_H
