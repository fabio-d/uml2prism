#ifndef GUI_UMLGRAPHICSITEM_H
#define GUI_UMLGRAPHICSITEM_H

#include <QGraphicsItem>

/* For each graphical element, there are three distinct objects:
 *  1) a Core::UMLElement (actually one of its subclasses)
 *  2) a Gui::UMLGraphicsItem (actually one of its subclasses)
 *  3) a QGraphicsItem (actually one of its subclasses)
 *
 * The Gui::UMLGraphicsItem object has methods to move to and from the other
 * two related objects.
 */

namespace Core
{
class UMLElement;
class UMLInitialNode;
}

namespace Gui
{

class UMLGraphicsItem
{
	public:
		// Get pointers from UMLGraphicsItem to the other two objects
		Core::UMLElement *coreItem() const;
		QGraphicsItem *qtItem() const;

		// Get pointer to the associated UMLGraphicsItem instance
		static UMLGraphicsItem *lookup(Core::UMLElement *coreItem);
		static UMLGraphicsItem *lookup(QGraphicsItem *qtItem);

		virtual ~UMLGraphicsItem();

	protected:
		UMLGraphicsItem();

		// Both bind methods must be called to initialize this object
		void bind(Core::UMLElement *coreItem);
		void bind(QGraphicsItem *qtItem);

	private:
		Core::UMLElement *m_coreItem;
		QGraphicsItem *m_qtItem;
};

class UMLGraphicsInitialNodeItem : public UMLGraphicsItem
{
	public:
		explicit UMLGraphicsInitialNodeItem(const QPointF &centerPosition);
		void bind(Core::UMLInitialNode *coreItem);

		QPointF centerPosition() const;

	private:
		static constexpr qreal Radius = 20;
		Core::UMLInitialNode *m_coreItem;
		QGraphicsEllipseItem *m_qtItem;
};

}

#endif // GUI_UMLGRAPHICSITEM_H
