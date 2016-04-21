#include "Gui/UMLElement.h"

#include "Gui/GraphicsLabelItem.h"

#include "Core/UMLElement.h"

#include <QDebug>
#include <QBrush>

enum class GraphicsItemDataKey : int
{
	UMLElementPtr // void* to corresponding Core::UMLElement object
};

static constexpr qreal InitialNodeRadius = 20;

static const QPolygonF DecisionMergeNodeShape = QPolygonF()
	<< QPointF(-30, 0) << QPointF(0, -30)
	<< QPointF(+30, 0) << QPointF(0, 30);

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

UMLInitialNode::UMLInitialNode(const QPointF &centerPosition)
{
	m_qtItem = new QGraphicsEllipseItem(
		-InitialNodeRadius / 2, -InitialNodeRadius / 2,
		InitialNodeRadius, InitialNodeRadius);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::black);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem, true);
	UMLElement::bind(m_qtItem);
}

void UMLInitialNode::bind(Core::UMLInitialNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

void UMLInitialNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLDecisionNode::UMLDecisionNode(const QPointF &centerPosition)
{
	m_qtItem = new QGraphicsPolygonItem(DecisionMergeNodeShape);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::white);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem);
	UMLElement::bind(m_qtItem);
}

void UMLDecisionNode::bind(Core::UMLDecisionNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

void UMLDecisionNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

UMLMergeNode::UMLMergeNode(const QPointF &centerPosition)
{
	m_qtItem = new QGraphicsPolygonItem(DecisionMergeNodeShape);
	m_qtItem->setPos(centerPosition);
	m_qtItem->setBrush(Qt::white);
	m_qtItem->setFlag(QGraphicsItem::ItemIsMovable, true);
	m_qtItem->setFlag(QGraphicsItem::ItemIsSelectable, true);

	m_labelItem = new GraphicsLabelItem(m_qtItem);
	UMLElement::bind(m_qtItem);
}

void UMLMergeNode::bind(Core::UMLMergeNode *coreItem)
{
	m_coreItem = coreItem;
	UMLElement::bind(coreItem);
}

void UMLMergeNode::refresh()
{
	Q_ASSERT(m_coreItem != nullptr);
	m_labelItem->setText(m_coreItem->nodeName());
}

}
