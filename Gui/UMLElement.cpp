#include "Gui/UMLElement.h"

#include "Gui/GraphicsAuxiliaryItems.h"

#include "Core/UMLElement.h"

#include <qmath.h>
#include <QDebug>
#include <QBrush>

enum class GraphicsItemDataKey : int
{
	UMLElementPtr // void* to corresponding Core::UMLElement object
};

static constexpr qreal InitialNodeRadius = 15;
static constexpr qreal FinalNodeRadius = InitialNodeRadius + InitialNodeRadius / 2;

static constexpr qreal ActionNodeRoundedCornerSize = 10;

static constexpr qreal DecisionMergeNodeHalfDiag = 30;
static const QPolygonF DecisionMergeNodeShape = QPolygonF()
	<< QPointF(-DecisionMergeNodeHalfDiag, 0) << QPointF(0, -DecisionMergeNodeHalfDiag)
	<< QPointF(+DecisionMergeNodeHalfDiag, 0) << QPointF(0, DecisionMergeNodeHalfDiag);

static constexpr qreal ForkJoinNodeHalfSideX = 30;
static constexpr qreal ForkJoinNodeHalfSideY = 5;
static const QRectF ForkJoinNodeShape = QRectF(
	-ForkJoinNodeHalfSideX, -ForkJoinNodeHalfSideY,
	ForkJoinNodeHalfSideX * 2, ForkJoinNodeHalfSideY * 2);

namespace Gui
{

UMLElement::UMLElement()
: m_coreItem(nullptr), m_qtItem(nullptr)
{
}

UMLElement::~UMLElement()
{
	qCritical() << "destr";
	delete m_qtItem;
}

void UMLElement::bind(Core::UMLElement *coreItem)
{
	Q_ASSERT(m_coreItem == nullptr);
	m_coreItem = coreItem;
	m_coreItem->setGuiProxyPointer(this);
}

void UMLElement::bind(QGraphicsItem *qtItem)
{
	Q_ASSERT(m_qtItem == nullptr);
	m_qtItem = qtItem;
	m_qtItem->setData((int)GraphicsItemDataKey::UMLElementPtr,
		QVariant::fromValue<void*>(this));
}

Core::UMLElement *UMLElement::coreItem() const
{
	Q_ASSERT(m_coreItem != nullptr);
	return m_coreItem;
}

QGraphicsItem *UMLElement::qtItem() const
{
	Q_ASSERT(m_qtItem != nullptr);
	return m_qtItem;
}

UMLElement *UMLElement::lookup(Core::UMLElement *coreItem)
{
	Q_ASSERT(coreItem->guiProxyPointer() != nullptr);
	return reinterpret_cast<UMLElement*>(coreItem->guiProxyPointer());
}

UMLElement *UMLElement::lookup(QGraphicsItem *qtItem, bool relaxed)
{
	if (relaxed)
	{
		// If we are querying a child node, we actually want to get info about
		// the main QGraphicsItem (i.e. tree root)
		while (qtItem->parentItem() != nullptr)
			qtItem = qtItem->parentItem();
	}

	QVariant data = qtItem->data((int)GraphicsItemDataKey::UMLElementPtr);
	void *value = data.value<void*>();

	if (value != nullptr)
		return reinterpret_cast<UMLElement*>(value);
	else
		return nullptr; // this may happen with auxiliary QGraphicsItems
}

void UMLElement::refresh()
{
}

QPointF UMLNodeElement::closestOutlinePoint(const QPointF &p)
{
	bool discard;
	return closestOutlinePoint(p, &discard);
}

QPointF UMLNodeElement::circleClosestPoint(const QPointF &centerPos,
			qreal radius, const QPointF &p, bool *out_pIsInside)
{
	const QPointF dir = p - centerPos;
	const qreal p_dist = qSqrt(dir.x()*dir.x() + dir.y()*dir.y());

	if (p_dist <= radius)
	{
		*out_pIsInside = true;
		if (p_dist == 0)
			return centerPos + QPointF(0, radius);
	}
	else
	{
		*out_pIsInside = false;
	}

	return centerPos + dir * radius / p_dist;
}

QPointF UMLNodeElement::segmentClosestPoint(const QPointF &segmA,
			const QPointF &segmB, const QPointF &p)
{
	const QPointF segmDir = segmB - segmA;
	const qreal segmLength = qSqrt(segmDir.x()*segmDir.x() + segmDir.y()*segmDir.y());
	const QPointF segmNormDir = segmDir / segmLength;
	const QPointF pointDir = p - segmA;

	qreal projDist = pointDir.x()*segmNormDir.x() + pointDir.y()*segmNormDir.y();
	if (projDist < 0)
		projDist = 0;
	else if (projDist > segmLength)
		projDist = segmLength;

	return segmA + segmNormDir * projDist;
}

QPointF UMLNodeElement::rectClosestPoint(const QRectF &rect, const QPointF &p,
	bool *out_pIsInside)
{
	QPointF candidates[4] = {
		segmentClosestPoint(rect.topLeft(), rect.topRight(), p),
		segmentClosestPoint(rect.topRight(), rect.bottomRight(), p),
		segmentClosestPoint(rect.bottomRight(), rect.bottomLeft(), p),
		segmentClosestPoint(rect.bottomLeft(), rect.topLeft(), p)
	};

	int minIdx = -1;
	qreal minDistSqr;
	for (int i = 0; i < 4; i++)
	{
		const QPointF distVec = candidates[i] - p;
		const qreal distSqr = distVec.x()*distVec.x() + distVec.y()*distVec.y();

		if (minIdx == -1 || distSqr < minDistSqr)
		{
			minIdx = i;
			minDistSqr = distSqr;
		}
	}

	*out_pIsInside = rect.contains(p);
	return candidates[minIdx];
}

UMLInitialNode::UMLInitialNode(const QPointF &centerPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsEllipseItem>(this,
		-InitialNodeRadius, -InitialNodeRadius,
		InitialNodeRadius * 2, InitialNodeRadius * 2);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::black);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem, GraphicsLabelItem::InitiallyOnTheRight);
	UMLElement::bind(m_qtItem);
}

void UMLInitialNode::bind(Core::UMLInitialNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

QPointF UMLInitialNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	return circleClosestPoint(m_qtItem->pos(), InitialNodeRadius,
		p, out_pIsInside);
}

void UMLInitialNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLFinalNode::UMLFinalNode(const QPointF &centerPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsEllipseItem>(this,
		-FinalNodeRadius, -FinalNodeRadius,
		FinalNodeRadius * 2, FinalNodeRadius * 2);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::white);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	QGraphicsEllipseItem *blackDot = new QGraphicsEllipseItem(
		-InitialNodeRadius, -InitialNodeRadius,
		InitialNodeRadius * 2, InitialNodeRadius * 2, m_qtItem);
	blackDot->setBrush(Qt::black);

	m_labelItem = new GraphicsLabelItem(m_qtItem, GraphicsLabelItem::InitiallyOnTheRight);
	UMLElement::bind(m_qtItem);
}

void UMLFinalNode::bind(Core::UMLFinalNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

QPointF UMLFinalNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	return circleClosestPoint(m_qtItem->pos(), FinalNodeRadius,
		p, out_pIsInside);
}

void UMLFinalNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLActionNode::UMLActionNode(const QPointF &centerPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsPathItem>(this);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::white);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	setRectPath(QSizeF());

	m_labelItem = new GraphicsLabelItem(m_qtItem, GraphicsLabelItem::NonMovable);
	UMLElement::bind(m_qtItem);
}

void UMLActionNode::bind(Core::UMLActionNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

QPointF UMLActionNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	const QRectF boundingRect = m_qtItem->path().controlPointRect().translated(m_qtItem->pos());
	const QRectF innerRect = boundingRect.adjusted(
		+ActionNodeRoundedCornerSize, +ActionNodeRoundedCornerSize,
		-ActionNodeRoundedCornerSize, -ActionNodeRoundedCornerSize);

	QPointF r = rectClosestPoint(boundingRect, p, out_pIsInside);
	if (r.x() < innerRect.left() && r.y() < innerRect.top())
		return circleClosestPoint(innerRect.topLeft(), ActionNodeRoundedCornerSize, p, out_pIsInside);
	else if (r.x() < innerRect.left() && r.y() > innerRect.bottom())
		return circleClosestPoint(innerRect.bottomLeft(), ActionNodeRoundedCornerSize, p, out_pIsInside);
	else if (r.x() > innerRect.right() && r.y() < innerRect.top())
		return circleClosestPoint(innerRect.topRight(), ActionNodeRoundedCornerSize, p, out_pIsInside);
	else if (r.x() > innerRect.right() && r.y() > innerRect.bottom())
		return circleClosestPoint(innerRect.bottomRight(), ActionNodeRoundedCornerSize, p, out_pIsInside);
	else
		return r;
}

void UMLActionNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
	setRectPath(m_labelItem->boundingRect().size());
}

void UMLActionNode::setRectPath(const QSizeF &size)
{
	const qreal width = size.width() + ActionNodeRoundedCornerSize * 2;
	const qreal height = size.height() + ActionNodeRoundedCornerSize * 2;

	QPainterPath p;
	p.addRoundedRect(-width/2, -height/2, width, height, ActionNodeRoundedCornerSize, ActionNodeRoundedCornerSize);
	m_qtItem->setPath(p);

	QGraphicsScene *sc = m_qtItem->scene();
	if (sc != nullptr)
		static_cast<UMLGraphicsScene*>(sc)->notifyGeometryChanged(this);
}

UMLDecisionNode::UMLDecisionNode(const QPointF &centerPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsPolygonItem>(this,
		DecisionMergeNodeShape);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::white);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem, GraphicsLabelItem::InitiallyOnTheBottom);
	UMLElement::bind(m_qtItem);
}

void UMLDecisionNode::bind(Core::UMLDecisionNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

QPointF UMLDecisionNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	const QPointF centerPos = m_qtItem->pos();
	*out_pIsInside = (p - centerPos).manhattanLength() <= DecisionMergeNodeHalfDiag;

	return segmentClosestPoint(
		centerPos + QPointF(DecisionMergeNodeHalfDiag * (p.x() < centerPos.x() ? -1 : +1), 0),
		centerPos + QPointF(0, DecisionMergeNodeHalfDiag * (p.y() < centerPos.y() ? -1 : +1)),
		p);
}

void UMLDecisionNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLMergeNode::UMLMergeNode(const QPointF &centerPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsPolygonItem>(this,
		DecisionMergeNodeShape);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::white);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem, GraphicsLabelItem::InitiallyOnTheBottom);
	UMLElement::bind(m_qtItem);
}

void UMLMergeNode::bind(Core::UMLMergeNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

QPointF UMLMergeNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	const QPointF centerPos = m_qtItem->pos();
	*out_pIsInside = (p - centerPos).manhattanLength() <= DecisionMergeNodeHalfDiag;

	return segmentClosestPoint(
		centerPos + QPointF(DecisionMergeNodeHalfDiag * (p.x() < centerPos.x() ? -1 : +1), 0),
		centerPos + QPointF(0, DecisionMergeNodeHalfDiag * (p.y() < centerPos.y() ? -1 : +1)),
		p);
}

void UMLMergeNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLForkNode::UMLForkNode(const QPointF &centerPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsRectItem>(this,
		ForkJoinNodeShape);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::black);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem, GraphicsLabelItem::InitiallyOnTheBottom);
	UMLElement::bind(m_qtItem);
}

void UMLForkNode::bind(Core::UMLForkNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

QPointF UMLForkNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	return rectClosestPoint(ForkJoinNodeShape.translated(m_qtItem->pos()),
		p, out_pIsInside);
}

void UMLForkNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLJoinNode::UMLJoinNode(const QPointF &centerPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsRectItem>(this,
		ForkJoinNodeShape);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::black);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem, GraphicsLabelItem::InitiallyOnTheBottom);
	UMLElement::bind(m_qtItem);
}

void UMLJoinNode::bind(Core::UMLJoinNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

QPointF UMLJoinNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	return rectClosestPoint(ForkJoinNodeShape.translated(m_qtItem->pos()),
		p, out_pIsInside);
}

void UMLJoinNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLControlFlowEdge::UMLControlFlowEdge()
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsLineItem>(this);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	UMLElement::bind(m_qtItem);
}

void UMLControlFlowEdge::bind(Core::UMLControlFlowEdge *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

void UMLControlFlowEdge::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_qtItem->setLine(calcLineBetweenNodes(
		static_cast<UMLNodeElement*>(lookup(m_coreItem->from())),
		static_cast<UMLNodeElement*>(lookup(m_coreItem->to()))
	));
}

QLineF UMLControlFlowEdge::calcLineBetweenNodes(UMLNodeElement *a, UMLNodeElement *b)
{
	QPointF midP = (a->qtItem()->pos() + b->qtItem()->pos()) / 2;
	return QLineF(a->closestOutlinePoint(midP), b->closestOutlinePoint(midP));
}

}
