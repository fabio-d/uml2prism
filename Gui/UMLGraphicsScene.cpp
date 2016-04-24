#include "Gui/UMLGraphicsScene.h"

#include "Gui/UMLElement.h"

#include "Core/UMLDiagram.h"
#include "Core/UMLElement.h"

#include <QAction>
#include <QApplication>
#include <QMenu>
#include <QDebug>
#include <QGraphicsSceneDragDropEvent>
#include <QInputDialog>
#include <QKeyEvent>
#include <QMimeData>
#include <QPainter>
#include <QToolBar>

namespace Gui
{

UMLGraphicsScene::UMLGraphicsScene(Core::UMLDiagram *dia, QObject *parent)
: QGraphicsScene(parent), m_dia(dia), m_createFlowOrigin(nullptr)
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

	m_dia->setGuiProxy(this);
}

UMLGraphicsScene::~UMLGraphicsScene()
{
	Q_ASSERT(items().isEmpty());
}

void UMLGraphicsScene::appendEditActions(QWidget *target)
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
		m_dia->deleteUMLElement(elem);
}

UMLNodeElement *UMLGraphicsScene::searchNodeElementAt(const QPointF &scenePos) const
{
	foreach (QGraphicsItem *qtItem, items(scenePos))
	{
		UMLElement *elem = UMLElement::lookup(qtItem, false);
		if (elem == nullptr)
			continue;

		Core::UMLElementType type = elem->coreItem()->type();
		if (type == Core::UMLElementType::InitialNode
			|| type == Core::UMLElementType::FinalNode
			|| type == Core::UMLElementType::ActionNode
			|| type == Core::UMLElementType::DecisionNode
			|| type == Core::UMLElementType::MergeNode
			|| type == Core::UMLElementType::ForkNode
			|| type == Core::UMLElementType::JoinNode)
		{
			return static_cast<UMLNodeElement*>(elem);
		}
	}

	return nullptr;
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

		QGraphicsItem *itemUnderMouse = itemsUnderMouse.isEmpty() ?
			nullptr : itemsUnderMouse.first();

		// select the item if selectable, otherwise try to see if its
		// parent can be selected
		while (itemUnderMouse != nullptr)
		{
			if (itemUnderMouse->flags().testFlag(QGraphicsItem::ItemIsSelectable))
			{
				itemUnderMouse->setSelected(true);
				break;
			}

			itemUnderMouse = itemUnderMouse->parentItem();
		}
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
	QList<QGraphicsItem*> itemsUnderMouse = items(event->scenePos());

	if (m_dia->type() == Core::UMLDiagram::Activity
		&& event->mimeData()->formats().contains("application/x-uml-create-node"))
	{
		event->setAccepted(true);
	}
	else if (m_dia->type() == Core::UMLDiagram::Activity
		&& event->mimeData()->formats().contains("application/x-uml-create-flow"))
	{
		event->setAccepted(searchNodeElementAt(event->scenePos()) != nullptr);
	}
	else if (m_dia->type() == Core::UMLDiagram::Class
		&& event->mimeData()->formats().contains("application/x-uml-create-datatype"))
	{
		event->setAccepted(true);
	}
	else
	{
		event->setAccepted(false);
	}
}

void UMLGraphicsScene::drawForeground(QPainter *painter, const QRectF &rect)
{
	painter->save();
	if (m_createFlowOrigin != nullptr)
	{
		UMLNodeElement *itemUnderMouse = searchNodeElementAt(m_mousePos);
		if (itemUnderMouse == nullptr)
		{
			QPolygonF path = m_intermediatePoints;
			path.append(m_mousePos);
			path.prepend(m_createFlowOrigin->closestOutlinePoint(path.first()));
			painter->drawPolyline(path);
		}
		else
		{
			painter->drawPolyline(UMLControlFlowEdge::calcPathBetweenNodes(
				m_createFlowOrigin,
				itemUnderMouse, m_intermediatePoints));
		}
	}
	painter->restore();
	QGraphicsScene::drawForeground(painter, rect);
}

void UMLGraphicsScene::dropEvent(QGraphicsSceneDragDropEvent *event)
{
	const QMimeData* mime = event->mimeData();
	const QPointF scenePos = event->scenePos();

	clearSelection();

	if (m_dia->type() == Core::UMLDiagram::Activity
		&& mime->formats().contains("application/x-uml-create-node"))
	{
		const QByteArray elementTypeString = mime->data("application/x-uml-create-node");

		if (elementTypeString == "InitialNode")
		{
			Core::UMLInitialNode *elem = new Core::UMLInitialNode();
			UMLInitialNode *item = new UMLInitialNode(scenePos);
			item->bind(elem);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "FinalNode")
		{
			Core::UMLFinalNode *elem = new Core::UMLFinalNode();
			UMLFinalNode *item = new UMLFinalNode(scenePos);
			item->bind(elem);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "ActionNode")
		{
			Core::UMLActionNode *elem = new Core::UMLActionNode();
			UMLActionNode *item = new UMLActionNode(scenePos);
			item->bind(elem);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "DecisionNode")
		{
			Core::UMLDecisionNode *elem = new Core::UMLDecisionNode();
			UMLDecisionNode *item = new UMLDecisionNode(scenePos);
			item->bind(elem);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "MergeNode")
		{
			Core::UMLMergeNode *elem = new Core::UMLMergeNode();
			UMLMergeNode *item = new UMLMergeNode(scenePos);
			item->bind(elem);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "ForkNode")
		{
			Core::UMLForkNode *elem = new Core::UMLForkNode();
			UMLForkNode *item = new UMLForkNode(scenePos);
			item->bind(elem);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "JoinNode")
		{
			Core::UMLJoinNode *elem = new Core::UMLJoinNode();
			UMLJoinNode *item = new UMLJoinNode(scenePos);
			item->bind(elem);

			m_dia->addUMLElement(elem);
		}
	}
	else if (m_dia->type() == Core::UMLDiagram::Activity
		&& mime->formats().contains("application/x-uml-create-flow"))
	{
		m_createFlowOrigin = searchNodeElementAt(scenePos);
		m_intermediatePoints.clear();
	}
	else if (m_dia->type() == Core::UMLDiagram::Class
		&& mime->formats().contains("application/x-uml-create-datatype"))
	{
		Core::UMLClass *elem = new Core::UMLClass();
		UMLClass *item = new UMLClass(scenePos);
		item->bind(elem);

		m_dia->addUMLElement(elem);
	}
}

void UMLGraphicsScene::mousePressEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	if (m_createFlowOrigin != nullptr)
	{
		UMLNodeElement *itemUnderMouse = searchNodeElementAt(mouseEvent->scenePos());

		if (itemUnderMouse != nullptr)
		{
			Core::UMLControlFlowEdge *elem = new Core::UMLControlFlowEdge(
				static_cast<Core::UMLNodeElement*>(m_createFlowOrigin->coreItem()),
				static_cast<Core::UMLNodeElement*>(itemUnderMouse->coreItem()));
			UMLControlFlowEdge *item = new UMLControlFlowEdge();
			item->bind(elem);
			item->setIntermediatePoints(m_intermediatePoints);

			m_dia->addUMLElement(elem);
			m_createFlowOrigin = nullptr;
			emit changed(QList<QRectF>());
		}
		else
		{
			m_intermediatePoints.append(mouseEvent->scenePos());
			emit changed(QList<QRectF>());
		}

		return;
	}

	QGraphicsScene::mousePressEvent(mouseEvent);
}

void UMLGraphicsScene::mouseMoveEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	if (m_createFlowOrigin)
	{
		m_mousePos = mouseEvent->scenePos();
		emit changed(QList<QRectF>());
	}

	QGraphicsScene::mouseMoveEvent(mouseEvent);
}

void UMLGraphicsScene::mouseReleaseEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	QGraphicsScene::mouseReleaseEvent(mouseEvent);
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

void UMLGraphicsScene::notifyGeometryChanged(UMLElement *element)
{
	Core::UMLElement *changedElement = element->coreItem();

	// Re-route edges
	foreach (QGraphicsItem *item, items())
	{
		UMLElement *e = UMLElement::lookup(item, false);
		if (e == nullptr)
			continue;

		switch (e->coreItem()->type())
		{
			case Core::UMLElementType::ControlFlowEdge:
			{
				Core::UMLControlFlowEdge *coreEdge =
					static_cast<Core::UMLControlFlowEdge*>(
						e->coreItem());

				if (coreEdge->from() == changedElement
					|| coreEdge->to() == changedElement)
				{
					e->refresh();
				}
				break;
			}
			default:
				// nop
				break;
		}
	}
}

}
