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
class UMLDecisionMergeNode;
class UMLEdgeElement;
class UMLElement;
class UMLEnumeration;
class UMLFinalNode;
class UMLForkJoinNode;
class UMLGlobalVariables;
class UMLInitialNode;
class UMLNodeElement;
class UMLSignalEdge;
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
		void refresh() override;

		// Returns shape's contour point that is closest to p. In the
		// two-arguments variant, *out_pIsInside is set to true if p in
		// contained in the shape
		QPointF closestOutlinePoint(const QPointF &p);
		virtual QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) = 0;

		void setLabelRelativePos(const QPointF &newPos);
		QPointF labelRelativePos() const;
		QSizeF labelSize() const;

		void resetLabelPosition();
		bool isLabelAtInitialPosition() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	protected:
		UMLNodeElement();

		// Helper methods that subclasses can use to implement closestOutlinePoint
		static QPointF circleClosestPoint(const QPointF &centerPos,
			qreal radius, const QPointF &p, bool *out_pIsInside);
		static QPointF segmentClosestPoint(const QPointF &segmA,
			const QPointF &segmB, const QPointF &p);
		static QPointF rectClosestPoint(const QRectF &rect,
			const QPointF &p, bool *out_pIsInside);

		void bind(Core::UMLElement *coreItem) = delete;
		void bind(Core::UMLNodeElement *coreItem);
		void bind(QGraphicsItem *qtItem) = delete;
		void bind(QGraphicsItem *qtItem, GraphicsLabelItem::Options labelOptions);

	private:
		Core::UMLNodeElement *m_coreItem;
		GraphicsLabelItem *m_labelItem;
};

class UMLInitialNode : public UMLNodeElement
{
	public:
		UMLInitialNode();
		void bind(Core::UMLInitialNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

	private:
		Core::UMLInitialNode *m_coreItem;
		QGraphicsEllipseItem *m_qtItem;
};

class UMLFinalNode : public UMLNodeElement
{
	public:
		UMLFinalNode();
		void bind(Core::UMLFinalNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

	private:
		Core::UMLFinalNode *m_coreItem;
		QGraphicsEllipseItem *m_qtItem;
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
};

class UMLDecisionMergeNode : public UMLNodeElement
{
	public:
		UMLDecisionMergeNode();
		void bind(Core::UMLDecisionMergeNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

	private:
		Core::UMLDecisionMergeNode *m_coreItem;
		QGraphicsPolygonItem *m_qtItem;
};

class UMLForkJoinNode : public UMLNodeElement
{
	public:
		UMLForkJoinNode();
		void bind(Core::UMLForkJoinNode *coreItem);
		QPointF closestOutlinePoint(const QPointF &p, bool *out_pIsInside) override;

	private:
		Core::UMLForkJoinNode *m_coreItem;
		QGraphicsRectItem *m_qtItem;
};

class UMLEdgeElement : public UMLElement, private GraphicsEdgeItem::MoveWatcher
{
	public:
		void setIntermediatePoints(const QPolygonF &intermediatePoints);
		const QPolygonF &intermediatePoints() const;

		void setLabelRelativePos(const QPointF &newPos);
		QPointF labelRelativePos() const;

		void refresh() override;
		void resetLabelPosition();
		bool isLabelAtInitialPosition() const;

		static QPolygonF calcPathBetweenNodes(UMLNodeElement *a, UMLNodeElement *b,
			const QPolygonF &intermediatePoints = QPolygonF());

		void storeToXml(QDomElement &target, QDomDocument &doc) const override;
		bool loadFromXml(const QDomElement &source) override;

	protected:
		UMLEdgeElement(qreal labelAtPercent, bool dottedLine);

		void bind(Core::UMLElement *coreItem) = delete;
		void bind(Core::UMLEdgeElement *coreItem);

		void setLabelText(const QString &text);

	private:
		void notifyEdgeMoved(const QPointF &delta) override;

		Core::UMLEdgeElement *m_coreItem;
		GraphicsEdgeItem *m_qtItem;
		GraphicsLabelItem *m_labelItem;
		QGraphicsPathItem *m_arrowItem;
		QPolygonF m_intermediatePoints;
};

class UMLControlFlowEdge : public UMLEdgeElement
{
	public:
		UMLControlFlowEdge();
		void bind(Core::UMLControlFlowEdge *coreItem);

		void refresh() override;

	private:
		Core::UMLControlFlowEdge *m_coreItem;
};

class UMLSignalEdge : public UMLEdgeElement
{
	public:
		UMLSignalEdge();
		void bind(Core::UMLSignalEdge *coreItem);

		void refresh() override;

	private:
		Core::UMLSignalEdge *m_coreItem;
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

class UMLGlobalVariables : public UMLElement
{
	public:
		UMLGlobalVariables();
		void bind(Core::UMLGlobalVariables *coreItem);

		void refresh() override;

	private:
		Core::UMLGlobalVariables *m_coreItem;
		GraphicsDatatypeItem *m_qtItem;
};

}

#endif // GUI_UMLELEMENT_H
