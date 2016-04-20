#include "Gui/GraphicsLabelItem.h"

#include <QFontMetricsF>
#include <QDebug>
namespace Gui
{

GraphicsLabelItem::GraphicsLabelItem(QGraphicsItem *parent, bool onTheRightByDefault)
: QGraphicsSimpleTextItem(parent)
{
	QFontMetricsF metrics(font());

	if (onTheRightByDefault)
		setPos(parent->boundingRect().right(), -metrics.height() / 2);
	else
		setPos(0, -metrics.height() / 2);

	setFlag(QGraphicsItem::ItemIsMovable, true);
	setFlag(QGraphicsItem::ItemIsSelectable, true);
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

}
