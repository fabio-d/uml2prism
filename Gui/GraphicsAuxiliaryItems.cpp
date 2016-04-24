#include "Gui/GraphicsAuxiliaryItems.h"

#include <QFontMetricsF>
#include <QPainter>

static constexpr qreal MinLineShapeWidth = 10;

const qreal DatatypeMargin = 7;

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

GraphicsDatatypeItem::GraphicsDatatypeItem(QGraphicsItem *parent)
{
	m_stereotype = new QGraphicsSimpleTextItem("<<example>>", this);
	m_name = new QGraphicsSimpleTextItem(this);
	m_contents = new QGraphicsSimpleTextItem(this);

	setBrush(Qt::white);

	QFont font = m_name->font();
	QFontMetricsF metrics(font);

	if (m_stereotype == nullptr)
	{
		m_contents->setPos(0, metrics.height() + 2*DatatypeMargin);
	}
	else
	{
		font.setItalic(true);
		m_stereotype->setFont(font);
		m_stereotype->setPos(-m_stereotype->boundingRect().size().width()/2, 0);
		m_name->setPos(0, metrics.height());
		m_contents->setPos(0, 2*metrics.height() + 2*DatatypeMargin);
	}

	relayout();
}

void GraphicsDatatypeItem::setName(const QString &text)
{
	m_name->setText(text);

	relayout();
}

void GraphicsDatatypeItem::setContents(const QString &text)
{
	m_contents->setText(text);

	relayout();
}

void GraphicsDatatypeItem::paint(QPainter *painter,
	const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	QGraphicsRectItem::paint(painter, option, widget);

	painter->setPen(pen());

	const qreal lineY = m_name->pos().y() + m_name->boundingRect().bottom() + DatatypeMargin;
	const qreal lineHalfWidth = rect().right();
	painter->drawLine(-lineHalfWidth, lineY, lineHalfWidth, lineY);
}

void GraphicsDatatypeItem::relayout()
{
	// Find longest line and total text height
	qreal longestWidth = 0;
	qreal totalHeight = 0;
	auto processTextBox = [&](const QSizeF &textSize)
	{
		longestWidth = qMax(longestWidth, textSize.width());
		totalHeight += textSize.height();
	};

	if (m_stereotype != nullptr)
		processTextBox(m_stereotype->boundingRect().size());
	processTextBox(m_name->boundingRect().size());
	processTextBox(m_contents->boundingRect().size());

	setRect(QRect(-longestWidth/2, 0, longestWidth, totalHeight)
		.adjusted(-DatatypeMargin, -DatatypeMargin, DatatypeMargin, 3*DatatypeMargin));

	m_name->setPos(
		-m_name->boundingRect().size().width()/2,
		m_name->pos().y());

	m_contents->setPos(
		-m_contents->boundingRect().size().width()/2,
		m_contents->pos().y());
}

}
