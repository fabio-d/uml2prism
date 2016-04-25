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
class UMLDecisionMergeNode;
class UMLEdgeElement;
class UMLElement;
class UMLEnumeration;
class UMLFinalNode;
class UMLForkJoinNode;
class UMLInitialNode;
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

		void setPos(const QPointF &newPos);
		QPointF pos() const;

		virtual void refresh();

		// serialize/deserialize gui properties
		virtual void storeToXml(QDomElement &target, QDomDocument &doc) const;
		virtual bool loadFromXml(const QDomElement &source);

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

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

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
		UMLInitialNode();
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
		UMLFinalNode();
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
		UMLActionNode();
		void bind(Core::UMLActionNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

		void refresh() override;

	private:
		void setRectPath(const QSizeF &size);

		Core::UMLActionNode *m_coreItem;
		QGraphicsPathItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLDecisionMergeNode : public UMLNodeElement
{
	public:
		UMLDecisionMergeNode();
		void bind(Core::UMLDecisionMergeNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

		void refresh() override;

	private:
		Core::UMLDecisionMergeNode *m_coreItem;
		QGraphicsPolygonItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLForkJoinNode : public UMLNodeElement
{
	public:
		UMLForkJoinNode();
		void bind(Core::UMLForkJoinNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

		void refresh() override;

	private:
		Core::UMLForkJoinNode *m_coreItem;
		QGraphicsRectItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLEdgeElement : public UMLElement, private GraphicsEdgeItem::MoveWatcher
{
	public:
		UMLEdgeElement();
		void bind(Core::UMLEdgeElement *coreItem);

		void setIntermediatePoints(const QPolygonF &intermediatePoints);
		const QPolygonF &intermediatePoints() const;

		void refresh() override;

		static QPolygonF calcPathBetweenNodes(UMLNodeElement *a, UMLNodeElement *b,
			const QPolygonF &intermediatePoints = QPolygonF());

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	private:
		void notifyEdgeMoved(const QPointF &delta) override;

		Core::UMLEdgeElement *m_coreItem;
		GraphicsEdgeItem *m_qtItem;
		GraphicsLabelItem *m_labelItemFrom;
		GraphicsLabelItem *m_labelItemTo;
		QGraphicsPathItem *m_arrowItem;
		QPolygonF m_intermediatePoints;
};

class UMLClass : public UMLElement
{
	public:
		UMLClass();
		void bind(Core::UMLClass *coreItem);

		void refresh() override;

	private:
		Core::UMLClass *m_coreItem;
		GraphicsDatatypeItem *m_qtItem;
};

class UMLEnumeration : public UMLElement
{
	public:
		UMLEnumeration();
		void bind(Core::UMLEnumeration *coreItem);

		void refresh() override;

	private:
		Core::UMLEnumeration *m_coreItem;
		GraphicsDatatypeItem *m_qtItem;
};

}

#endif // GUI_UMLELEMENT_H
