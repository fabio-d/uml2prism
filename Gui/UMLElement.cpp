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

static constexpr qreal ArrowShapeSideX = 20;
static constexpr qreal ArrowShapeSideY = 20;
static const QPolygonF ArrowShape = QPolygonF()
	<< QPointF(-ArrowShapeSideX, -ArrowShapeSideY/2) << QPointF(0, 0)
	<< QPointF(-ArrowShapeSideX, +ArrowShapeSideY/2);

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

	m_labelItem = new GraphicsLabelItem(GraphicsLabelItem::InitiallyOnTheRight, m_qtItem);
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

	m_labelItem = new GraphicsLabelItem(GraphicsLabelItem::InitiallyOnTheRight, m_qtItem);
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

	m_labelItem = new GraphicsLabelItem(GraphicsLabelItem::NonMovable, m_qtItem);
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

UMLDecisionMergeNode::UMLDecisionMergeNode(const QPointF &centerPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsPolygonItem>(this,
		DecisionMergeNodeShape);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::white);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(GraphicsLabelItem::InitiallyOnTheBottom, m_qtItem);
	UMLElement::bind(m_qtItem);
}

void UMLDecisionMergeNode::bind(Core::UMLDecisionMergeNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

QPointF UMLDecisionMergeNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	const QPointF centerPos = m_qtItem->pos();
	*out_pIsInside = (p - centerPos).manhattanLength() <= DecisionMergeNodeHalfDiag;

	return segmentClosestPoint(
		centerPos + QPointF(DecisionMergeNodeHalfDiag * (p.x() < centerPos.x() ? -1 : +1), 0),
		centerPos + QPointF(0, DecisionMergeNodeHalfDiag * (p.y() < centerPos.y() ? -1 : +1)),
		p);
}

void UMLDecisionMergeNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLForkJoinNode::UMLForkJoinNode(const QPointF &centerPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsRectItem>(this,
		ForkJoinNodeShape);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::black);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(GraphicsLabelItem::InitiallyOnTheBottom, m_qtItem);
	UMLElement::bind(m_qtItem);
}

void UMLForkJoinNode::bind(Core::UMLForkJoinNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

QPointF UMLForkJoinNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	return rectClosestPoint(ForkJoinNodeShape.translated(m_qtItem->pos()),
		p, out_pIsInside);
}

void UMLForkJoinNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLEdgeElement::UMLEdgeElement()
{
	m_qtItem = new GraphicsEdgeItem();
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);
	m_qtItem->setWatcher(this);

	m_labelItemFrom = new GraphicsLabelItem(GraphicsLabelItem::NoOptions, m_qtItem->createPlaceholder(.2));
	m_labelItemTo = new GraphicsLabelItem(GraphicsLabelItem::NoOptions, m_qtItem->createPlaceholder(.6));

	QPainterPath path;
	path.addPolygon(ArrowShape);
	m_arrowItem = new QGraphicsPathItem(path, m_qtItem);

	UMLElement::bind(m_qtItem);
}

void UMLEdgeElement::bind(Core::UMLEdgeElement *coreItem)
{
	m_coreItem = coreItem;
	if (m_coreItem->type() == Core::UMLElementType::SignalEdge)
	{
		m_qtItem->setPen(Qt::DashDotLine);
		m_arrowItem->setPen(Qt::DashDotLine);
	}

	UMLElement::bind(coreItem);
}

void UMLEdgeElement::setIntermediatePoints(const QPolygonF &intermediatePoints)
{
	m_intermediatePoints = intermediatePoints;
	refresh();

	QGraphicsScene *sc = m_qtItem->scene();
	if (sc != nullptr)
		static_cast<UMLGraphicsScene*>(sc)->notifyGeometryChanged(this);
}

void UMLEdgeElement::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);

	QPolygonF path = calcPathBetweenNodes(
		static_cast<UMLNodeElement*>(lookup(m_coreItem->from())),
		static_cast<UMLNodeElement*>(lookup(m_coreItem->to())),
		m_intermediatePoints);

	path.translate(-m_qtItem->pos());
	m_qtItem->setPolyline(path);
	m_arrowItem->setPos(path.last());
	m_arrowItem->setRotation(-m_qtItem->path().angleAtPercent(1));

	m_labelItemFrom->setText("[from]");
	m_labelItemTo->setText("[to]");
}

void UMLEdgeElement::notifyEdgeMoved(const QPointF &delta)
{
	setIntermediatePoints(m_intermediatePoints.translated(delta));
}

QPolygonF UMLEdgeElement::calcPathBetweenNodes(UMLNodeElement *a, UMLNodeElement *b, const QPolygonF &intermediatePoints)
{
	QPolygonF res;

	if (intermediatePoints.isEmpty())
	{
		QPointF midP = (a->qtItem()->pos() + b->qtItem()->pos()) / 2;
		res.append(a->closestOutlinePoint(midP));
		res.append(b->closestOutlinePoint(midP));
	}
	else
	{
		res.append(a->closestOutlinePoint(intermediatePoints.first()));
		res += intermediatePoints;
		res.append(b->closestOutlinePoint(intermediatePoints.last()));
	}

	return res;
}

UMLClass::UMLClass(const QPointF &topMidPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<GraphicsDatatypeItem>(this, false);
	m_qtItem->setPos(topMidPosition);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	UMLElement::bind(m_qtItem);
}

void UMLClass::bind(Core::UMLClass *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

void UMLClass::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_qtItem->setName(m_coreItem->datatypeName());
	m_qtItem->setContents("Bla\nblabla\nblablablablablablablablabla");
}

UMLEnumeration::UMLEnumeration(const QPointF &topMidPosition)
{
	m_qtItem = new GraphicsPositionChangeSpyItem<GraphicsDatatypeItem>(this, true);
	m_qtItem->setPos(topMidPosition);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	UMLElement::bind(m_qtItem);
}

void UMLEnumeration::bind(Core::UMLEnumeration *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

void UMLEnumeration::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_qtItem->setName(m_coreItem->datatypeName());
	m_qtItem->setContents("Bla\nblabla\nblablablablablablablablabla");
}

}
