#ifndef CORE_UMLDIAGRAM_H
#define CORE_UMLDIAGRAM_H

#include <QObject>
#include <QList>

namespace Core
{

class GuiProxy;
class UMLElement;

class UMLDiagram : public QObject
{
	Q_OBJECT

	public:
		enum Type { Activity, Class };

		UMLDiagram(Type type);
		~UMLDiagram();

		Type type() const;

		// The GuiProxy object receives notifications about events
		void setGuiProxy(GuiProxy *guiProxy);
		GuiProxy *guiProxy() const;

		// Add/delete elements (delete methods also destroy the object)
		void addUMLElement(UMLElement *element);
		void deleteUMLElement(UMLElement *element);
		void deleteAllElements();

		// Generate a name that is not currently in use by any element
		QString generateFreshName(const QString &prefix);

	private slots:
		void slotElementChanged();

	private:
		Type m_type;
		GuiProxy *m_guiProxy;

		// sorted by type
		QList<UMLElement*> m_elements;
};

}

#endif // CORE_UMLDIAGRAM_H
