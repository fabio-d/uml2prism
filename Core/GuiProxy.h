#ifndef CORE_GUIPROXY_H
#define CORE_GUIPROXY_H

namespace Core
{

class UMLElement;

class GuiProxy
{
	public:
		virtual void notifyElementAdded(UMLElement *element) = 0;
		virtual void notifyElementRemoved(UMLElement *element) = 0;
};

}

#endif // CORE_GUIPROXY_H
