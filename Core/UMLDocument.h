#ifndef CORE_UMLDOCUMENT_H
#define CORE_UMLDOCUMENT_H

#include <QObject>
#include <QList>

namespace Core
{

class GuiProxy;
class UMLElement;

class UMLDocument : public QObject
{
	Q_OBJECT

	public:
		UMLDocument();
		~UMLDocument();

		// The GuiProxy object receives notifications about events
		void setGuiProxy(GuiProxy *guiProxy);
		GuiProxy *guiProxy() const;

		// Add/delete elements (delete methods also destroy the object)
		void addUMLElement(UMLElement *element);
		bool deleteUMLElement(UMLElement *element);
		void deleteAllElements();

	private slots:
		void slotElementChanged();

	private:
		GuiProxy *m_guiProxy;

		// sorted by type
		QList<UMLElement*> m_elements;
};

}

#endif // CORE_UMLDOCUMENT_H
