#include "Gui/GraphicsAuxiliaryItems.h"

#include <QFontMetricsF>

static constexpr qreal MinLineShapeWidth = 10;

namespace Gui
{

GraphicsLabelItem::GraphicsLabelItem(Options options, QGraphicsItem *parent)
: QGraphicsSimpleTextItem(parent)
{
	QFontMetricsF metrics(font());

	if (options.testFlag(InitiallyOnTheRight))
		setPos(parent->boundingRect().right(), -metrics.height() / 2);
	else if (options.testFlag(InitiallyOnTheBottom))
		setPos(0, parent->boundingRect().bottom());
	else /* initially centered */
		setPos(0, -metrics.height() / 2);

	if (!options.testFlag(NonMovable))
	{
		setFlag(QGraphicsItem::ItemIsMovable, true);
		setFlag(QGraphicsItem::ItemIsSelectable, true);
	}
}

void GraphicsLabelItem::setText(const QString &text)
{
	setVisible(!text.isEmpty());

	const qreal oldWidth = boundingRect().width();
	const qreal oldCenter = mapToParent(boundingRect().center()).x();

	QGraphicsSimpleTextItem::setText(text);

	const qreal newWidth = boundingRect().width();

	if (oldCenter <= parentItem()->boundingRect().left())
	{
		// right aligned
		setPos(pos().x() + oldWidth - newWidth, pos().y());
	}
	else if (oldCenter < parentItem()->boundingRect().right())
	{
		// center aligned
		setPos(pos().x() + (oldWidth - newWidth) / 2, pos().y());
	}
}

GraphicsEdgeItem::GraphicsEdgeItem(QGraphicsItem *parent)
: QGraphicsLineItem(parent)
{
	setZValue(1);
}

QRectF GraphicsEdgeItem::boundingRect() const
{
	const QRectF minRect(
		-MinLineShapeWidth/2, -MinLineShapeWidth/2,
		MinLineShapeWidth, MinLineShapeWidth);
	const QRectF baseRect = QGraphicsLineItem::boundingRect();
	return baseRect | minRect.translated(baseRect.center());
}

QPainterPath GraphicsEdgeItem::shape() const
{
	QPainterPathStroker stk;
	stk.setWidth(MinLineShapeWidth);
	return stk.createStroke(QGraphicsLineItem::shape());
}

QGraphicsItem *GraphicsEdgeItem::createPlaceholder(qreal atPercentage)
{
	QGraphicsRectItem *ph = new QGraphicsRectItem(-1, -1, 2, 2, this);
	ph->setPen(QPen(Qt::transparent));
	m_placeholders.insert(atPercentage, ph);
	ph->setPos(line().pointAt(atPercentage));
	return ph;
}

void GraphicsEdgeItem::setLine(const QLineF &line)
{
	QGraphicsLineItem::setLine(line);

	for (QMultiMap<qreal, QGraphicsItem*>::const_iterator it = m_placeholders.begin();
		it != m_placeholders.end(); ++it)
	{
		it.value()->setPos(line.pointAt(it.key()));
	}
}

void GraphicsEdgeItem::setLine(qreal x1, qreal y1, qreal x2, qreal y2)
{
	setLine(QLine(x1, y1, x2, y2));
}

}
