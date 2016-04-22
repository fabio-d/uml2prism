#include "Gui/UMLGraphicsScene.h"

#include "Gui/UMLElement.h"

#include "Core/UMLDocument.h"
#include "Core/UMLElement.h"

#include <QAction>
#include <QApplication>
#include <QMenu>
#include <QDebug>
#include <QInputDialog>
#include <QKeyEvent>
#include <QMimeData>
#include <QGraphicsSceneDragDropEvent>
#include <QToolBar>

namespace Gui
{

UMLGraphicsScene::UMLGraphicsScene(Core::UMLDocument *doc, QObject *parent)
: QGraphicsScene(parent), m_doc(doc)
{
	m_actionRenameNode = new QAction(
		QIcon(":/kde_icons/resources/kde_icons/edit-rename.png"),
		"Rename node", this);
	m_actionRenameNode->setShortcut(Qt::Key_F2);
	connect(m_actionRenameNode, SIGNAL(triggered()), this, SLOT(slotRenameNode()));

	m_actionDeleteSelection = new QAction(
		QIcon(":/kde_icons/resources/kde_icons/edit-delete.png"),
		"Delete selection", this);
	m_actionDeleteSelection->setShortcut(QKeySequence::Delete);
	connect(m_actionDeleteSelection, SIGNAL(triggered()), this, SLOT(slotDeleteSelection()));

	connect(this, SIGNAL(selectionChanged()), this, SLOT(slotSelectionChanged()));
	slotSelectionChanged();

	m_doc->setGuiProxy(this);
}

UMLGraphicsScene::~UMLGraphicsScene()
{
	Q_ASSERT(items().isEmpty());
}

void UMLGraphicsScene::addActions(QMenu *target)
{
	target->addAction(m_actionRenameNode);
	target->addAction(m_actionDeleteSelection);
}

void UMLGraphicsScene::addActions(QToolBar *target)
{
	target->addAction(m_actionRenameNode);
	target->addAction(m_actionDeleteSelection);
}

void UMLGraphicsScene::slotSelectionChanged()
{
	m_strictSelection.clear();
	m_relaxedSelection.clear();

	foreach (QGraphicsItem *qtItem, selectedItems())
	{
		// Attemp strict lookup
		UMLElement *item = UMLElement::lookup(qtItem, false);
		if (item != nullptr)
			m_strictSelection.append(item);
		else // attempt relaxed lookup
			item = UMLElement::lookup(qtItem);

		if (!m_relaxedSelection.contains(item))
			m_relaxedSelection.append(item);
	}

	if (m_relaxedSelection.count() == 1)
	{
		Core::UMLElement *elem = m_relaxedSelection[0]->coreItem();
		Core::UMLNodeElement *nodeElem = dynamic_cast<Core::UMLNodeElement*>(elem);
		m_actionRenameNode->setEnabled(nodeElem != nullptr);
	}
	else
	{
		m_actionRenameNode->setEnabled(false);
	}

	m_actionDeleteSelection->setEnabled((m_relaxedSelection.count() != 0 &&
		m_relaxedSelection.count() == m_strictSelection.count()) ||
		m_relaxedSelection.count() == 1);
}

void UMLGraphicsScene::slotRenameNode()
{
	if (m_relaxedSelection.count() != 1)
		return;

	Core::UMLElement *elem = m_relaxedSelection[0]->coreItem();
	Core::UMLNodeElement *nodeElem = dynamic_cast<Core::UMLNodeElement*>(elem);

	if (!nodeElem)
		return;

	bool ok;
	const QString newLabel = QInputDialog::getText(
		QApplication::activeWindow(), "Rename node", "New label",
		QLineEdit::Normal, nodeElem->nodeName(), &ok);

	if (ok)
		nodeElem->setNodeName(newLabel);
}

void UMLGraphicsScene::slotDeleteSelection()
{
	QList<Core::UMLElement*> elementsToBeDeleted;

	foreach (UMLElement *item, m_relaxedSelection)
		elementsToBeDeleted.append(item->coreItem());

	// Sort elementsToBeDeleted according to their type so
	// that references are never broken during the deletion process
	Core::UMLElement::topoSort(elementsToBeDeleted, true);

	foreach (Core::UMLElement *elem, elementsToBeDeleted)
		m_doc->deleteUMLElement(elem);
}

void UMLGraphicsScene::contextMenuEvent(QGraphicsSceneContextMenuEvent *contextMenuEvent)
{
	QList<QGraphicsItem*> itemsUnderMouse = items(contextMenuEvent->scenePos());
	bool atLeastOneItemIsSelected = false;
	foreach (QGraphicsItem *qtItem, selectedItems())
	{
		if (itemsUnderMouse.contains(qtItem))
		{
			atLeastOneItemIsSelected = true;
			break;
		}
	}

	// If the user clicked on a non-selected item, clear current selection
	// and select that item
	if (atLeastOneItemIsSelected == false)
	{
		if (!(contextMenuEvent->modifiers() & Qt::ControlModifier))
			clearSelection();
		if (!itemsUnderMouse.isEmpty())
			itemsUnderMouse[0]->setSelected(true);
	}

	QMenu menu;
	if (m_actionRenameNode->isEnabled())
		menu.addAction(m_actionRenameNode);
	if (m_actionDeleteSelection->isEnabled())
		menu.addAction(m_actionDeleteSelection);

	if (!menu.isEmpty())
		menu.exec(contextMenuEvent->screenPos());
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
		UMLInitialNode *item = new UMLInitialNode(scenePos);
		item->bind(elem);

		m_doc->addUMLElement(elem);
	}
	else if (elementTypeString == "DecisionNode")
	{
		Core::UMLDecisionNode *elem = new Core::UMLDecisionNode();
		UMLDecisionNode *item = new UMLDecisionNode(scenePos);
		item->bind(elem);

		m_doc->addUMLElement(elem);
	}
	else if (elementTypeString == "MergeNode")
	{
		Core::UMLMergeNode *elem = new Core::UMLMergeNode();
		UMLMergeNode *item = new UMLMergeNode(scenePos);
		item->bind(elem);

		m_doc->addUMLElement(elem);
	}
	else if (elementTypeString == "ForkNode")
	{
		Core::UMLForkNode *elem = new Core::UMLForkNode();
		UMLForkNode *item = new UMLForkNode(scenePos);
		item->bind(elem);

		m_doc->addUMLElement(elem);
	}
	else if (elementTypeString == "JoinNode")
	{
		Core::UMLJoinNode *elem = new Core::UMLJoinNode();
		UMLJoinNode *item = new UMLJoinNode(scenePos);
		item->bind(elem);

		m_doc->addUMLElement(elem);
	}
	else if (elementTypeString == "FinalNode")
	{
		Core::UMLFinalNode *elem = new Core::UMLFinalNode();
		UMLFinalNode *item = new UMLFinalNode(scenePos);
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

void UMLGraphicsScene::notifyElementAdded(Core::UMLElement *element)
{
	UMLElement *item = UMLElement::lookup(element);
	item->refresh();
	addItem(item->qtItem());
}

void UMLGraphicsScene::notifyElementChanged(Core::UMLElement *element)
{
	UMLElement *item = UMLElement::lookup(element);
	item->refresh();
}

void UMLGraphicsScene::notifyElementRemoved(Core::UMLElement *element)
{
	UMLElement *item = UMLElement::lookup(element);
	removeItem(item->qtItem());
	delete item;
}

}
