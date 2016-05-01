#include "Gui/GraphicsAuxiliaryItems.h"

#include "Gui/UMLGraphicsScene.h"

#include <QFontMetricsF>
#include <QDebug>
#include <QPainter>
#include <QTextDocument>

static constexpr qreal MinLineShapeWidth = 10;

const qreal DatatypeMargin = 7;

const qreal CommentMargin = 17;

namespace Gui
{

GraphicsLabelItem::GraphicsLabelItem(Options options, QGraphicsItem *parent)
: QGraphicsSimpleTextItem(parent), m_options(options)
{
	setFont(UMLGraphicsScene::sceneFont());

	QFontMetricsF metrics(font());

	if (m_options.testFlag(InitiallyOnTheRight))
		setPos(parent->boundingRect().right(), -metrics.height() / 2);
	else if (m_options.testFlag(InitiallyOnTheBottom))
		setPos(0, parent->boundingRect().bottom());
	else /* initially centered */
		setPos(0, -metrics.height() / 2);

	if (!m_options.testFlag(NonMovable))
	{
		setFlag(QGraphicsItem::ItemIsMovable, true);
		setFlag(QGraphicsItem::ItemIsSelectable, true);
	}
}

QPointF GraphicsLabelItem::calcInitialPosition() const
{
	const qreal width = boundingRect().width();
	QFontMetricsF metrics(font());

	if (m_options.testFlag(InitiallyOnTheRight))
		return QPointF(parentItem()->boundingRect().right(), -metrics.height() / 2);
	else if (m_options.testFlag(InitiallyOnTheBottom))
		return QPointF(-width / 2, parentItem()->boundingRect().bottom());
	else /* initially centered */
		return QPointF(-width / 2, -metrics.height() / 2);
}

void GraphicsLabelItem::resetPosition()
{
	setPos(calcInitialPosition());
}

bool GraphicsLabelItem::isAtInitialPosition() const
{
	return pos() == calcInitialPosition();
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

GraphicsLabelItem::Options GraphicsLabelItem::options() const
{
	return m_options;
}

GraphicsEdgeItem::GraphicsEdgeItem(QGraphicsItem *parent)
: QGraphicsPathItem(parent), m_watcher(nullptr), m_lastKnownPos(0, 0)
{
	setFlag(QGraphicsItem::ItemIsMovable, true);
	setFlag(QGraphicsItem::ItemSendsGeometryChanges, true);
	setZValue(1);
}

QRectF GraphicsEdgeItem::boundingRect() const
{
	const QRectF minRect(
		-MinLineShapeWidth/2, -MinLineShapeWidth/2,
		MinLineShapeWidth, MinLineShapeWidth);
	const QRectF baseRect = QGraphicsPathItem::boundingRect();
	return baseRect | minRect.translated(baseRect.center());
}

QPainterPath GraphicsEdgeItem::shape() const
{
	QPainterPathStroker stk;
	stk.setWidth(MinLineShapeWidth);
	return stk.createStroke(QGraphicsPathItem::shape());
}

QGraphicsItem *GraphicsEdgeItem::createPlaceholder(qreal atPercent)
{
	QGraphicsRectItem *ph = new QGraphicsRectItem(-1, -1, 2, 2, this);
	ph->setPen(QPen(Qt::transparent));
	m_placeholders.insert(atPercent, ph);
	ph->setPos(path().pointAtPercent(atPercent));
	return ph;
}

void GraphicsEdgeItem::setWatcher(MoveWatcher *watcher)
{
	m_watcher = watcher;
}

void GraphicsEdgeItem::setPolyline(const QPolygonF &polyline)
{
	QPainterPath path;
	path.addPolygon(polyline);
	QGraphicsPathItem::setPath(path);

	for (QMultiMap<qreal, QGraphicsItem*>::const_iterator it = m_placeholders.begin();
		it != m_placeholders.end(); ++it)
	{
		it.value()->setPos(path.pointAtPercent(it.key()));
	}
}

QVariant GraphicsEdgeItem::itemChange(GraphicsItemChange change, const QVariant &value)
{
	if (change == ItemPositionHasChanged)
	{
		const QPointF newPos = value.toPointF();

		if (m_watcher != nullptr)
			m_watcher->notifyEdgeMoved(newPos - m_lastKnownPos);

		m_lastKnownPos = newPos;
	}

	return QGraphicsPathItem::itemChange(change, value);
}

GraphicsDatatypeItem::Entry::Entry()
: highlightError(false)
{
}

GraphicsDatatypeItem::Entry::Entry(const QString &text, bool underline, bool highlightError)
: text(text), underline(underline), highlightError(highlightError)
{
}

GraphicsDatatypeItem::GraphicsDatatypeItem(Options options, QGraphicsItem *parent)
{
	if (options.testFlag(EnumerationStereotype))
		m_stereotype = new QGraphicsSimpleTextItem(QString::fromUtf8(u8"\u00ABenumeration\u00BB"), this);
	else if (options.testFlag(GlobalStereotype))
		m_stereotype = new QGraphicsSimpleTextItem(QString::fromUtf8(u8"\u00ABglobal\u00BB"), this);
	else
		m_stereotype = nullptr;

	m_name = new QGraphicsSimpleTextItem(this);
	m_contents = new QGraphicsTextItem(this);

	setBrush(Qt::white);

	QFont font = UMLGraphicsScene::sceneFont();
	QFontMetricsF metrics(font);
	m_contents->setFont(font);

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
		if (options.testFlag(GlobalStereotype))
			m_contents->setPos(0, metrics.height() + 2*DatatypeMargin);
		else
			m_contents->setPos(0, 2*metrics.height() + 2*DatatypeMargin);
	}

	font.setBold(true);
	font.setItalic(false);
	m_name->setFont(font);

	relayout();
}

void GraphicsDatatypeItem::setName(const QString &text)
{
	m_name->setText(text);

	relayout();
}

void GraphicsDatatypeItem::setEntries(const QList<Entry> &contents)
{
	QString html;

	foreach (const Entry &entry, contents)
	{
		html += "<div style=\"";
		if (entry.underline)
			html += "text-decoration: underline;";
		if (entry.highlightError)
			html += "color: red;";
		html += "\">";
		html += Qt::escape(entry.text);
		html += "</div>";
	}

	m_contents->setHtml(html);

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
		-longestWidth/2,
		m_contents->pos().y());
}

GraphicsCommentItem::GraphicsCommentItem(QGraphicsItem *parent)
: QGraphicsPolygonItem(parent)
{
	setBrush(Qt::white);
	setZValue(-1);

	m_contents = new QGraphicsSimpleTextItem(this);

	QFont font = UMLGraphicsScene::sceneFont();
	font.setFamily(QString());
	font.setStyleHint(QFont::TypeWriter);
	font.setPixelSize(font.pixelSize() / 2);
	m_contents->setFont(font);

	setText(QString());
	setFlag(QGraphicsItem::ItemIsSelectable, true);
	setFlag(QGraphicsItem::ItemIsMovable, true);
}

void GraphicsCommentItem::setText(const QString &text)
{
	m_contents->setText(text);
	const QSizeF textSize = m_contents->boundingRect().size();
	m_contents->setPos(-textSize.width() / 2, -textSize.height() / 2);
	setShapeSize(QSizeF(
		textSize.width() + 2*CommentMargin,
		textSize.height() + 2*CommentMargin));
}

void GraphicsCommentItem::paint(QPainter *painter,
	const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	QGraphicsPolygonItem::paint(painter, option, widget);

	painter->setPen(pen());

	const QPolygonF p = QPolygonF()
		<< QPointF(+m_shapeSize.width() / 2, -m_shapeSize.height() / 2 + CommentMargin)
		<< QPointF(+m_shapeSize.width() / 2 - CommentMargin, -m_shapeSize.height() / 2 + CommentMargin)
		<< QPointF(+m_shapeSize.width() / 2 - CommentMargin, -m_shapeSize.height() / 2);
	painter->drawPolyline(p);
}

void GraphicsCommentItem::setShapeSize(const QSizeF &size)
{
	const QPolygonF p = QPolygonF()
		<< QPointF(-size.width() / 2, -size.height() / 2)
		<< QPointF(-size.width() / 2, +size.height() / 2)
		<< QPointF(+size.width() / 2, +size.height() / 2)
		<< QPointF(+size.width() / 2, -size.height() / 2 + CommentMargin)
		<< QPointF(+size.width() / 2 - CommentMargin, -size.height() / 2);
	m_shapeSize = size;
	setPolygon(p);
}

}
