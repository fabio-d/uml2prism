#include "Gui/UMLElement.h"

#include "Gui/GraphicsAuxiliaryItems.h"

#include "Core/UMLElement.h"

#include <qmath.h>
#include <QDebug>
#include <QDomDocument>
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

void UMLElement::setPos(const QPointF &newPos)
{
	m_qtItem->setPos(newPos);
}

QPointF UMLElement::pos() const
{
	return m_qtItem->pos();
}

void UMLElement::refresh()
{
}

void UMLElement::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	target.setAttribute("x", pos().x());
	target.setAttribute("y", pos().y());
}

bool UMLElement::loadFromXml(const QDomElement &source)
{
	// Labels must be set to their final text *before* restoring their
	// position, to avoid confusing the alignment code
	refresh();

	setPos(QPointF(source.attribute("x").toDouble(),
		source.attribute("y").toDouble()));
	return true;
}

UMLNodeElement::UMLNodeElement()
: m_labelItem(nullptr)
{
}

void UMLNodeElement::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

void UMLNodeElement::bind(Core::UMLNodeElement *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

void UMLNodeElement::bind(QGraphicsItem *qtItem, GraphicsLabelItem::Options labelOptions)
{
	m_labelItem = new GraphicsPositionChangeSpyItem<GraphicsLabelItem>(
		this, labelOptions, qtItem);
	UMLElement::bind(qtItem);
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

void UMLNodeElement::setLabelRelativePos(const QPointF &newPos)
{
	m_labelItem->setPos(newPos);
}

QPointF UMLNodeElement::labelRelativePos() const
{
	return m_labelItem->pos();
}

QSizeF UMLNodeElement::labelSize() const
{
	return m_labelItem->boundingRect().size();
}

void UMLNodeElement::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	UMLElement::storeToXml(target, doc); // store position

	if (!m_labelItem->options().testFlag(GraphicsLabelItem::NonMovable))
	{
		QDomElement labelElem = doc.createElement("label");
		target.appendChild(labelElem);
		labelElem.setAttribute("x", labelRelativePos().x());
		labelElem.setAttribute("y", labelRelativePos().y());
	}
}

bool UMLNodeElement::loadFromXml(const QDomElement &source)
{
	if (!UMLElement::loadFromXml(source)) // load position
		return false;

	if (!m_labelItem->options().testFlag(GraphicsLabelItem::NonMovable))
	{
		QDomElement labelElem = source.firstChildElement("label");
		setLabelRelativePos(QPointF(labelElem.attribute("x").toDouble(),
			labelElem.attribute("y").toDouble()));
	}

	return true;
}

UMLInitialNode::UMLInitialNode()
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsEllipseItem>(this,
		-InitialNodeRadius, -InitialNodeRadius,
		InitialNodeRadius * 2, InitialNodeRadius * 2);
	m_qtItem->setBrush(Qt::black);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	UMLNodeElement::bind(m_qtItem, GraphicsLabelItem::InitiallyOnTheRight);
}

void UMLInitialNode::bind(Core::UMLInitialNode *coreItem)
{
	m_coreItem = coreItem;
	UMLNodeElement::bind(coreItem);
}

QPointF UMLInitialNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	return circleClosestPoint(m_qtItem->pos(), InitialNodeRadius,
		p, out_pIsInside);
}

UMLFinalNode::UMLFinalNode()
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsEllipseItem>(this,
		-FinalNodeRadius, -FinalNodeRadius,
		FinalNodeRadius * 2, FinalNodeRadius * 2);
	m_qtItem->setBrush(Qt::white);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	QGraphicsEllipseItem *blackDot = new QGraphicsEllipseItem(
		-InitialNodeRadius, -InitialNodeRadius,
		InitialNodeRadius * 2, InitialNodeRadius * 2, m_qtItem);
	blackDot->setBrush(Qt::black);

	UMLNodeElement::bind(m_qtItem, GraphicsLabelItem::InitiallyOnTheRight);
}

void UMLFinalNode::bind(Core::UMLFinalNode *coreItem)
{
	m_coreItem = coreItem;
	UMLNodeElement::bind(coreItem);
}

QPointF UMLFinalNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	return circleClosestPoint(m_qtItem->pos(), FinalNodeRadius,
		p, out_pIsInside);
}

UMLActionNode::UMLActionNode()
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsPathItem>(this);
	m_qtItem->setBrush(Qt::white);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	setRectPath(QSizeF());

	UMLNodeElement::bind(m_qtItem, GraphicsLabelItem::NonMovable);
}

void UMLActionNode::bind(Core::UMLActionNode *coreItem)
{
	m_coreItem = coreItem;
	UMLNodeElement::bind(coreItem);
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
	UMLNodeElement::refresh();
	setRectPath(labelSize());
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

UMLDecisionMergeNode::UMLDecisionMergeNode()
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsPolygonItem>(this,
		DecisionMergeNodeShape);
	m_qtItem->setBrush(Qt::white);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	UMLNodeElement::bind(m_qtItem, GraphicsLabelItem::InitiallyOnTheBottom);
}

void UMLDecisionMergeNode::bind(Core::UMLDecisionMergeNode *coreItem)
{
	m_coreItem = coreItem;
	UMLNodeElement::bind(coreItem);
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

UMLForkJoinNode::UMLForkJoinNode()
{
	m_qtItem = new GraphicsPositionChangeSpyItem<QGraphicsRectItem>(this,
		ForkJoinNodeShape);
	m_qtItem->setBrush(Qt::black);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	UMLNodeElement::bind(m_qtItem,GraphicsLabelItem::InitiallyOnTheBottom);
}

void UMLForkJoinNode::bind(Core::UMLForkJoinNode *coreItem)
{
	m_coreItem = coreItem;
	UMLNodeElement::bind(coreItem);
}

QPointF UMLForkJoinNode::closestOutlinePoint(const QPointF &p, bool *out_pIsInside)
{
	return rectClosestPoint(ForkJoinNodeShape.translated(m_qtItem->pos()),
		p, out_pIsInside);
}

UMLEdgeElement::UMLEdgeElement(qreal labelAtPercent, bool dottedLine)
{
	m_qtItem = new GraphicsEdgeItem();
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);
	m_qtItem->setWatcher(this);

	m_labelItem = new GraphicsPositionChangeSpyItem<GraphicsLabelItem>(this,
		GraphicsLabelItem::NoOptions, m_qtItem->createPlaceholder(labelAtPercent));

	QPainterPath path;
	path.addPolygon(ArrowShape);
	m_arrowItem = new QGraphicsPathItem(path, m_qtItem);

	if (dottedLine)
	{
		m_qtItem->setPen(Qt::DashDotLine);
		m_arrowItem->setPen(Qt::DashDotLine);
	}

	UMLElement::bind(m_qtItem);
}

void UMLEdgeElement::bind(Core::UMLEdgeElement *coreItem)
{
	m_coreItem = coreItem;
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

const QPolygonF &UMLEdgeElement::intermediatePoints() const
{
	return m_intermediatePoints;
}

void UMLEdgeElement::setLabelRelativePos(const QPointF &newPos)
{
	m_labelItem->setPos(newPos);
}

QPointF UMLEdgeElement::labelRelativePos() const
{
	return m_labelItem->pos();
}

void UMLEdgeElement::setLabelText(const QString &text)
{
	if (text.isEmpty())
	{
		setLabelRelativePos(QPointF(0, 0));
		m_labelItem->setVisible(false);
	}
	else
	{
		m_labelItem->setText(text);
		m_labelItem->setVisible(true);
	}
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

void UMLEdgeElement::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	QDomElement intermediatePointsArrayElem = doc.createElement("intermediate-points");
	target.appendChild(intermediatePointsArrayElem);

	foreach (const QPointF &point, intermediatePoints())
	{
		QDomElement pointElem = doc.createElement("point");
		intermediatePointsArrayElem.appendChild(pointElem);
		pointElem.setAttribute("x", point.x());
		pointElem.setAttribute("y", point.y());
	}

	const QPointF labelRelativePos = m_labelItem->pos();
	QDomElement labelElem = doc.createElement("label");
	target.appendChild(labelElem);
	labelElem.setAttribute("x", labelRelativePos.x());
	labelElem.setAttribute("y", labelRelativePos.y());
}

bool UMLEdgeElement::loadFromXml(const QDomElement &source)
{
	QPolygonF intermediatePoints;

	QDomElement intermediatePointsArrayElem = source.firstChildElement("intermediate-points");
	for (QDomElement pointElem = intermediatePointsArrayElem.firstChildElement();
		!pointElem.isNull();
		pointElem = pointElem.nextSiblingElement())
	{
		intermediatePoints.append(QPointF(
			pointElem.attribute("x").toDouble(),
			pointElem.attribute("y").toDouble()));
	}

	setIntermediatePoints(intermediatePoints);

	QDomElement labelElem = source.firstChildElement("label");
	m_labelItem->setPos(labelElem.attribute("x").toDouble(),
		labelElem.attribute("y").toDouble());

	return true;
}

UMLControlFlowEdge::UMLControlFlowEdge()
: UMLEdgeElement(.2, false)
{
}

void UMLControlFlowEdge::bind(Core::UMLControlFlowEdge *coreItem)
{
	m_coreItem = coreItem;
	UMLEdgeElement::bind(coreItem);
}

void UMLControlFlowEdge::refresh()
{
	UMLEdgeElement::refresh();

	const QString &branchName = m_coreItem->branchName();
	setLabelText(branchName.isEmpty() ? "" : QString("[%1]").arg(branchName));
}

UMLSignalEdge::UMLSignalEdge()
: UMLEdgeElement(.6, true)
{
}

void UMLSignalEdge::bind(Core::UMLSignalEdge *coreItem)
{
	m_coreItem = coreItem;
	UMLEdgeElement::bind(coreItem);
}

void UMLSignalEdge::refresh()
{
	UMLEdgeElement::refresh();
	setLabelText(m_coreItem->signalName());
}

UMLClass::UMLClass()
{
	m_qtItem = new GraphicsPositionChangeSpyItem<GraphicsDatatypeItem>(this,
		GraphicsDatatypeItem::NoOptions);
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

	QList<QPair<bool, QString>> valuesWithErrorState;
	foreach (const Core::UMLClass::MemberVariable &var, m_coreItem->memberVariables())
		valuesWithErrorState.append(QPair<bool, QString>(false,
			QString("%1 : %2").arg(var.name).arg(var.datatypeName.toString())));

	m_qtItem->setEntries(valuesWithErrorState);
}

UMLEnumeration::UMLEnumeration()
{
	m_qtItem = new GraphicsPositionChangeSpyItem<GraphicsDatatypeItem>(this,
		GraphicsDatatypeItem::EnumerationStereotype);
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

	const QStringList &sourceValues = m_coreItem->values();
	QList<QPair<bool, QString>> valuesWithErrorState;

	for (int i = 0; i < sourceValues.count(); i++)
		valuesWithErrorState.append(QPair<bool, QString>(false, sourceValues[i]));

	m_qtItem->setEntries(valuesWithErrorState);
}

UMLGlobalVariables::UMLGlobalVariables()
{
	m_qtItem = new GraphicsPositionChangeSpyItem<GraphicsDatatypeItem>(this,
		GraphicsDatatypeItem::GlobalStereotype);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	UMLElement::bind(m_qtItem);
}

void UMLGlobalVariables::bind(Core::UMLGlobalVariables *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

void UMLGlobalVariables::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_qtItem->setEntries(QList<QPair<bool, QString>>()
		<< QPair<bool, QString>(false, "good")
		<< QPair<bool, QString>(true, "bad")
		<< QPair<bool, QString>(false, "good again")
	);
}

}
