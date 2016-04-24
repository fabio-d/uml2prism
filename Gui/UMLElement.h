#ifndef GUI_UMLELEMENT_H
#define GUI_UMLELEMENT_H

#include "Gui/GraphicsAuxiliaryItems.h"

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
class UMLClass;
class UMLControlFlowEdge;
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

class UMLNodeElement : public UMLElement
{
	public:
		// Returns shape's contour point that is closest to p. In the
		// two-arguments variant, *out_pIsInside is set to true if p in
		// contained in the shape
		QPointF closestOutlinePoint(const QPointF &p);
		virtual QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) = 0;

	protected:
		UMLNodeElement() = default;

		// Helper methods that subclasses can use to implement closestOutlinePoint
		static QPointF circleClosestPoint(const QPointF &centerPos,
			qreal radius, const QPointF &p, bool *out_pIsInside);
		static QPointF segmentClosestPoint(const QPointF &segmA,
			const QPointF &segmB, const QPointF &p);
		static QPointF rectClosestPoint(const QRectF &rect,
			const QPointF &p, bool *out_pIsInside);
};

class UMLInitialNode : public UMLNodeElement
{
	public:
		explicit UMLInitialNode(const QPointF &centerPosition);
		void bind(Core::UMLInitialNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

		void refresh() override;

	private:
		Core::UMLInitialNode *m_coreItem;
		QGraphicsEllipseItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLFinalNode : public UMLNodeElement
{
	public:
		explicit UMLFinalNode(const QPointF &centerPosition);
		void bind(Core::UMLFinalNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

		void refresh() override;

	private:
		Core::UMLFinalNode *m_coreItem;
		QGraphicsEllipseItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLActionNode : public UMLNodeElement
{
	public:
		explicit UMLActionNode(const QPointF &centerPosition);
		void bind(Core::UMLActionNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

		void refresh() override;

	private:
		void setRectPath(const QSizeF &size);

		Core::UMLActionNode *m_coreItem;
		QGraphicsPathItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLDecisionNode : public UMLNodeElement
{
	public:
		explicit UMLDecisionNode(const QPointF &centerPosition);
		void bind(Core::UMLDecisionNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

		void refresh() override;

	private:
		Core::UMLDecisionNode *m_coreItem;
		QGraphicsPolygonItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLMergeNode : public UMLNodeElement
{
	public:
		explicit UMLMergeNode(const QPointF &centerPosition);
		void bind(Core::UMLMergeNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

		void refresh() override;

	private:
		Core::UMLMergeNode *m_coreItem;
		QGraphicsPolygonItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLForkNode : public UMLNodeElement
{
	public:
		explicit UMLForkNode(const QPointF &centerPosition);
		void bind(Core::UMLForkNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

		void refresh() override;

	private:
		Core::UMLForkNode *m_coreItem;
		QGraphicsRectItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLJoinNode : public UMLNodeElement
{
	public:
		explicit UMLJoinNode(const QPointF &centerPosition);
		void bind(Core::UMLJoinNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

		void refresh() override;

	private:
		Core::UMLJoinNode *m_coreItem;
		QGraphicsRectItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLControlFlowEdge : public UMLElement, private GraphicsEdgeItem::MoveWatcher
{
	public:
		UMLControlFlowEdge();
		void bind(Core::UMLControlFlowEdge *coreItem);

		void setIntermediatePoints(const QPolygonF &intermediatePoints);
		void refresh() override;

		static QPolygonF calcPathBetweenNodes(UMLNodeElement *a, UMLNodeElement *b,
			const QPolygonF &intermediatePoints = QPolygonF());

	private:
		void notifyEdgeMoved(const QPointF &delta) override;

		Core::UMLControlFlowEdge *m_coreItem;
		GraphicsEdgeItem *m_qtItem;
		GraphicsLabelItem *m_labelItemFrom;
		GraphicsLabelItem *m_labelItemTo;
		QGraphicsPathItem *m_arrowItem;
		QPolygonF m_intermediatePoints;
};

class UMLClass : public UMLElement
{
	public:
		explicit UMLClass(const QPointF &topMidPosition);
		void bind(Core::UMLClass *coreItem);

		void refresh() override;

	private:
		Core::UMLClass *m_coreItem;
		GraphicsDatatypeItem *m_qtItem;
};

}

#endif // GUI_UMLELEMENT_H
