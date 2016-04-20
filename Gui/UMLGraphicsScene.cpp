#include "Gui/UMLGraphicsScene.h"

#include "Gui/UMLGraphicsItem.h"

#include "Core/UMLDocument.h"
#include "Core/UMLElement.h"

#include <QKeyEvent>
#include <QMimeData>
#include <QGraphicsSceneDragDropEvent>

#include <QGraphicsSimpleTextItem>
namespace Gui
{

UMLGraphicsScene::UMLGraphicsScene(Core::UMLDocument *doc, QObject *parent)
: QGraphicsScene(parent), m_doc(doc)
{
	m_doc->setGuiProxy(this);

	connect(this, &QGraphicsScene::changed, [=]()
	{
		//setSceneRect(itemsBoundingRect());
	});
}

UMLGraphicsScene::~UMLGraphicsScene()
{
	Q_ASSERT(items().isEmpty());
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
	const QByteArray elementTypeString = mime->data("application/x-uml-create-element");

	if (elementTypeString == "InitialNode")
	{
		Core::UMLInitialNode *elem = new Core::UMLInitialNode();
		UMLGraphicsInitialNodeItem *item = new UMLGraphicsInitialNodeItem(scenePos);
		item->bind(elem);

		m_doc->addUMLElement(elem);
	}
	else
	{
		QGraphicsSimpleTextItem *item = addSimpleText(
			elementTypeString);
		item->setPos(scenePos);
		item->setFlag(QGraphicsItem::ItemIsMovable, true);
		item->setFlag(QGraphicsItem::ItemIsSelectable, true);
	}
}

void UMLGraphicsScene::keyPressEvent(QKeyEvent *keyEvent)
{
	if (keyEvent->matches(QKeySequence::Delete))
	{
		QList<Core::UMLElement*> elementsToBeDeleted;

		foreach (QGraphicsItem *qtItem, selectedItems())
		{
			UMLGraphicsItem *item = UMLGraphicsItem::lookup(qtItem);
			elementsToBeDeleted.append(item->coreItem());
		}

		// Sort elementsToBeDeleted according to their type so
		// that references are never broken during the deletion process
		Core::UMLElement::topoSort(elementsToBeDeleted, true);

		foreach (Core::UMLElement *elem, elementsToBeDeleted)
			m_doc->deleteUMLElement(elem);
	}
	else
	{
		QGraphicsScene::keyPressEvent(keyEvent);
	}
}

void UMLGraphicsScene::notifyElementAdded(Core::UMLElement *element)
{
	addItem(UMLGraphicsItem::lookup(element)->qtItem());
}

void UMLGraphicsScene::notifyElementChanged(Core::UMLElement *element)
{
	// TODO
}

void UMLGraphicsScene::notifyElementRemoved(Core::UMLElement *element)
{
	UMLGraphicsItem *item = UMLGraphicsItem::lookup(element);
	removeItem(item->qtItem());
	delete item;
}

}
