#include "Gui/UMLGraphicsItem.h"

#include "Gui/GraphicsLabelItem.h"

#include "Core/UMLElement.h"

#include <QDebug>
#include <QBrush>

enum class GraphicsItemDataKey : int
{
	UMLGraphicsItemPtr // void* to corresponding UMLGraphicsItem object
};

static constexpr qreal InitialNodeRadius = 20;

static const QPolygonF DecisionMergeNodeShape = QPolygonF()
	<< QPointF(-30, 0) << QPointF(0, -30)
	<< QPointF(+30, 0) << QPointF(0, 30);

namespace Gui
{

UMLGraphicsItem::UMLGraphicsItem()
: m_coreItem(nullptr), m_qtItem(nullptr)
{
}

UMLGraphicsItem::~UMLGraphicsItem()
{
	qCritical() << "destr";
	delete m_qtItem;
}

void UMLGraphicsItem::bind(Core::UMLElement *coreItem)
{
	Q_ASSERT(m_coreItem == nullptr);
	m_coreItem = coreItem;
	m_coreItem->setGuiProxyPointer(this);
}

void UMLGraphicsItem::bind(QGraphicsItem *qtItem)
{
	Q_ASSERT(m_qtItem == nullptr);
	m_qtItem = qtItem;
	m_qtItem->setData((int)GraphicsItemDataKey::UMLGraphicsItemPtr,
		QVariant::fromValue<void*>(this));
}

Core::UMLElement *UMLGraphicsItem::coreItem() const
{
	Q_ASSERT(m_coreItem != nullptr);
	return m_coreItem;
}

QGraphicsItem *UMLGraphicsItem::qtItem() const
{
	Q_ASSERT(m_qtItem != nullptr);
	return m_qtItem;
}

UMLGraphicsItem *UMLGraphicsItem::lookup(Core::UMLElement *coreItem)
{
	Q_ASSERT(coreItem->guiProxyPointer() != nullptr);
	return reinterpret_cast<UMLGraphicsItem*>(coreItem->guiProxyPointer());
}

UMLGraphicsItem *UMLGraphicsItem::lookup(QGraphicsItem *qtItem)
{
	QVariant data = qtItem->data((int)GraphicsItemDataKey::UMLGraphicsItemPtr);
	void *value = data.value<void*>();

	if (value != nullptr)
		return reinterpret_cast<UMLGraphicsItem*>(value);
	else
		return nullptr; // this may happen with auxiliary QGraphicsItems
}

void UMLGraphicsItem::refresh()
{
}

UMLGraphicsInitialNodeItem::UMLGraphicsInitialNodeItem(const QPointF &centerPosition)
{
	m_qtItem = new QGraphicsEllipseItem(
		-InitialNodeRadius / 2, -InitialNodeRadius / 2,
		InitialNodeRadius, InitialNodeRadius);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::black);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem, true);
	UMLGraphicsItem::bind(m_qtItem);
}

void UMLGraphicsInitialNodeItem::bind(Core::UMLInitialNode *coreItem)
{
	m_coreItem = coreItem;
	UMLGraphicsItem::bind(coreItem);
}

void UMLGraphicsInitialNodeItem::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLGraphicsDecisionNodeItem::UMLGraphicsDecisionNodeItem(const QPointF &centerPosition)
{
	m_qtItem = new QGraphicsPolygonItem(DecisionMergeNodeShape);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem);
	UMLGraphicsItem::bind(m_qtItem);
}

void UMLGraphicsDecisionNodeItem::bind(Core::UMLDecisionNode *coreItem)
{
	m_coreItem = coreItem;
	UMLGraphicsItem::bind(coreItem);
}

void UMLGraphicsDecisionNodeItem::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLGraphicsMergeNodeItem::UMLGraphicsMergeNodeItem(const QPointF &centerPosition)
{
	m_qtItem = new QGraphicsPolygonItem(DecisionMergeNodeShape);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem);
	UMLGraphicsItem::bind(m_qtItem);
}

void UMLGraphicsMergeNodeItem::bind(Core::UMLMergeNode *coreItem)
{
	m_coreItem = coreItem;
	UMLGraphicsItem::bind(coreItem);
}

void UMLGraphicsMergeNodeItem::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

}
