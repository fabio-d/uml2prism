#ifndef GUI_UMLGRAPHICSITEM_H
#define GUI_UMLGRAPHICSITEM_H

#include <QGraphicsItem>

/* For each graphical element, there are at least three distinct objects:
 *  1) a Core::UMLElement (actually one of its subclasses)
 *  2) a Gui::UMLGraphicsItem (actually one of its subclasses)
 *  3) the main QGraphicsItem (actually one of its subclasses)
 *
 * The Gui::UMLGraphicsItem object has methods to move to and from the other
 * two related objects.
 *
 * Some nodes also have auxiliary QGraphicsItem that are not tracked by the
 * Gui::UMLGraphicsItem object.
 */

namespace Core
{
class UMLDecisionNode;
class UMLMergeNode;
class UMLElement;
class UMLInitialNode;
}

namespace Gui
{

class GraphicsLabelItem;

class UMLGraphicsItem
{
	public:
		// Get pointers from UMLGraphicsItem to the other two objects
		Core::UMLElement *coreItem() const;
		QGraphicsItem *qtItem() const;

		// Get pointer to the associated UMLGraphicsItem instance
		static UMLGraphicsItem *lookup(Core::UMLElement *coreItem);
		static UMLGraphicsItem *lookup(QGraphicsItem *qtItem);

		virtual void refresh();

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

		void refresh() override;

	private:
		Core::UMLInitialNode *m_coreItem;
		QGraphicsEllipseItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLGraphicsDecisionNodeItem : public UMLGraphicsItem
{
	public:
		explicit UMLGraphicsDecisionNodeItem(const QPointF &centerPosition);
		void bind(Core::UMLDecisionNode *coreItem);

		void refresh() override;

	private:
		Core::UMLDecisionNode *m_coreItem;
		QGraphicsPolygonItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLGraphicsMergeNodeItem : public UMLGraphicsItem
{
	public:
		explicit UMLGraphicsMergeNodeItem(const QPointF &centerPosition);
		void bind(Core::UMLMergeNode *coreItem);

		void refresh() override;

	private:
		Core::UMLMergeNode *m_coreItem;
		QGraphicsPolygonItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

}

#endif // GUI_UMLGRAPHICSITEM_H
