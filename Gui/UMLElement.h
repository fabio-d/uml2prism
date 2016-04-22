#ifndef GUI_UMLELEMENT_H
#define GUI_UMLELEMENT_H

#include <QGraphicsItem>

/* For each graphical element, there are at least three distinct objects:
 *  1) a Core::UMLElement (actually one of its subclasses)
 *  2) a Gui::UMLElement (actually one of its subclasses)
 *  3) the main QGraphicsItem (actually one of its subclasses)
 *
 * The Gui::UMLElement object has methods to move to and from the other
 * two related objects.
 *
 * Nodes may also have auxiliary child QGraphicsItem objects that are not
 * directly tracked by the Gui::UMLElement object.
 */

namespace Core
{
class UMLActionNode;
class UMLDecisionNode;
class UMLElement;
class UMLFinalNode;
class UMLForkNode;
class UMLJoinNode;
class UMLInitialNode;
class UMLMergeNode;
}

namespace Gui
{

class GraphicsLabelItem;

class UMLElement
{
	public:
		// Get pointers from UMLElement to the other two objects
		Core::UMLElement *coreItem() const;
		QGraphicsItem *qtItem() const;

		// Get pointer to the associated UMLElement instance.
		// If relaxed is set, lookup(QGraphicsItem*) also works with
		// child nodes by climbing up to the root QGraphicsItem
		static UMLElement *lookup(Core::UMLElement *coreItem);
		static UMLElement *lookup(QGraphicsItem *qtItem, bool relaxed = true);

		virtual void refresh();

		virtual ~UMLElement();

	protected:
		UMLElement();

		// Both bind methods must be called to initialize this object
		void bind(Core::UMLElement *coreItem);
		void bind(QGraphicsItem *qtItem);

	private:
		Core::UMLElement *m_coreItem;
		QGraphicsItem *m_qtItem;
};

class UMLInitialNode : public UMLElement
{
	public:
		explicit UMLInitialNode(const QPointF &centerPosition);
		void bind(Core::UMLInitialNode *coreItem);

		void refresh() override;

	private:
		Core::UMLInitialNode *m_coreItem;
		QGraphicsEllipseItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLFinalNode : public UMLElement
{
	public:
		explicit UMLFinalNode(const QPointF &centerPosition);
		void bind(Core::UMLFinalNode *coreItem);

		void refresh() override;

	private:
		Core::UMLFinalNode *m_coreItem;
		QGraphicsEllipseItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLActionNode : public UMLElement
{
	public:
		explicit UMLActionNode(const QPointF &centerPosition);
		void bind(Core::UMLActionNode *coreItem);

		void refresh() override;

	private:
		void setRectPath(const QSizeF &size);

		Core::UMLActionNode *m_coreItem;
		QGraphicsPathItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLDecisionNode : public UMLElement
{
	public:
		explicit UMLDecisionNode(const QPointF &centerPosition);
		void bind(Core::UMLDecisionNode *coreItem);

		void refresh() override;

	private:
		Core::UMLDecisionNode *m_coreItem;
		QGraphicsPolygonItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLMergeNode : public UMLElement
{
	public:
		explicit UMLMergeNode(const QPointF &centerPosition);
		void bind(Core::UMLMergeNode *coreItem);

		void refresh() override;

	private:
		Core::UMLMergeNode *m_coreItem;
		QGraphicsPolygonItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLForkNode : public UMLElement
{
	public:
		explicit UMLForkNode(const QPointF &centerPosition);
		void bind(Core::UMLForkNode *coreItem);

		void refresh() override;

	private:
		Core::UMLForkNode *m_coreItem;
		QGraphicsRectItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLJoinNode : public UMLElement
{
	public:
		explicit UMLJoinNode(const QPointF &centerPosition);
		void bind(Core::UMLJoinNode *coreItem);

		void refresh() override;

	private:
		Core::UMLJoinNode *m_coreItem;
		QGraphicsRectItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

}

#endif // GUI_UMLELEMENT_H
