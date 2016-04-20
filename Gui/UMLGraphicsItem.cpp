#include "Gui/UMLGraphicsItem.h"

#include "Core/UMLElement.h"

#include <QDebug>
#include <QBrush>

enum class GraphicsItemDataKey : int
{
	UMLGraphicsItemPtr // void* to corresponding UMLGraphicsItem object
};

namespace Gui
{

UMLGraphicsItem::UMLGraphicsItem()
: m_coreItem(nullptr), m_qtItem(nullptr)
{
}

UMLGraphicsItem::~UMLGraphicsItem()
{
	qCritical() << "destr";
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
	Q_ASSERT(data.value<void*>() != nullptr);
	return reinterpret_cast<UMLGraphicsItem*>(data.value<void*>());
}

UMLGraphicsInitialNodeItem::UMLGraphicsInitialNodeItem(const QPointF &centerPosition)
{
	m_qtItem = new QGraphicsEllipseItem(
		centerPosition.x() - Radius / 2,
		centerPosition.y() - Radius / 2,
		Radius, Radius);
	m_qtItem->setBrush(Qt::black);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	UMLGraphicsItem::bind(m_qtItem);
}

void UMLGraphicsInitialNodeItem::bind(Core::UMLInitialNode *coreItem)
{
	m_coreItem = coreItem;
	UMLGraphicsItem::bind(coreItem);
}

QPointF UMLGraphicsInitialNodeItem::centerPosition() const
{
	return m_qtItem->rect().center();
}

}
