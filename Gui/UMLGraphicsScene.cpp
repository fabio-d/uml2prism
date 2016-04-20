#include "UMLGraphicsScene.h"

#include <QMimeData>
#include <QGraphicsSceneDragDropEvent>

#include <QGraphicsSimpleTextItem>
namespace Gui
{

UMLGraphicsScene::UMLGraphicsScene(QObject *parent)
: QGraphicsScene(parent)
{
	connect(this, &QGraphicsScene::changed, [=]()
	{
		//setSceneRect(itemsBoundingRect());
	});
}

void UMLGraphicsScene::dragMoveEvent(QGraphicsSceneDragDropEvent *event)
{
	if (event->mimeData()->formats().contains("application/x-uml-create-element"))
	{
		event->setAccepted(true);
	}
	else
	{
		event->setAccepted(false);
	}
}

void UMLGraphicsScene::dropEvent(QGraphicsSceneDragDropEvent *event)
{
	const QMimeData* mime = event->mimeData();
	const QPointF scenePos = event->scenePos();

	QGraphicsSimpleTextItem *item = addSimpleText(
		mime->data("application/x-uml-create-element"));
	item->setPos(scenePos);
	item->setFlag(QGraphicsItem::ItemIsMovable, true);
	item->setFlag(QGraphicsItem::ItemIsSelectable, true);
}

}
