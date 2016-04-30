#include "Gui/UMLGraphicsScene.h"

#include "Gui/EditClassDialog.h"
#include "Gui/EditEnumerationDialog.h"
#include "Gui/EditGlobalVariablesDialog.h"
#include "Gui/EditScriptedNodeElementDialog.h"
#include "Gui/EditSignalEdgeDialog.h"
#include "Gui/RenameDialog.h"
#include "Gui/UMLElement.h"

#include "Core/Document.h"
#include "Core/UMLDiagram.h"
#include "Core/UMLElement.h"

#include <QAction>
#include <QApplication>
#include <QDebug>
#include <QGraphicsSceneDragDropEvent>
#include <QInputDialog>
#include <QKeyEvent>
#include <QMenu>
#include <QMimeData>
#include <QPainter>
#include <QTimer>
#include <QToolBar>

namespace Gui
{

UMLGraphicsScene::UMLGraphicsScene(Core::UMLDiagram *dia, QObject *parent)
: QGraphicsScene(parent), m_dia(dia), m_edgeConstructionOrigin(nullptr),
  m_somethingWasMoved(false), m_undoIsAlreadyScheduled(false)
{
	connect(this, SIGNAL(selectionChanged()), this, SLOT(slotSelectionChanged()));
	slotSelectionChanged();

	m_dia->setGuiProxy(this);
}

UMLGraphicsScene::~UMLGraphicsScene()
{
	Q_ASSERT(items().isEmpty());
}

const QFont &UMLGraphicsScene::sceneFont()
{
	// All texts must be drawn using this font, because scene units must not
	// depend on the current display's DPI. Note that what is called "pixel"
	// here is actually one scene unit.
	static QFont font = ({ QFont f = QApplication::font(); f.setPixelSize(27); f; });
	return font;
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

	bool editEnabled, renameEnabled, deleteEnabled;
	if (m_relaxedSelection.count() == 1)
	{
		Core::UMLElement *elem = m_relaxedSelection[0]->coreItem();
		editEnabled = canBeEdited(elem);
		renameEnabled = canBeRenamed(elem);
	}
	else
	{
		editEnabled = false;
		renameEnabled = false;
	}

	deleteEnabled = (m_relaxedSelection.count() != 0 &&
		m_relaxedSelection.count() == m_strictSelection.count()) ||
		m_relaxedSelection.count() == 1;

	emit actionsEnabledChanged(editEnabled, renameEnabled, deleteEnabled);
}

void UMLGraphicsScene::renameSelectedItem(QWidget *requestingWidget)
{
	Q_ASSERT(m_relaxedSelection.count() == 1);

	RenameDialog diag(m_relaxedSelection[0]->coreItem(), requestingWidget);
	diag.exec();
}

void UMLGraphicsScene::editSelectedItem(QWidget *requestingWidget)
{
	Q_ASSERT(m_relaxedSelection.count() == 1);

	Core::UMLElement *elem = m_relaxedSelection[0]->coreItem();
	Core::UMLScriptedNodeElement *scriptedNodeElem = dynamic_cast<Core::UMLScriptedNodeElement*>(elem);
	Core::UMLSignalEdge *signalEdgeElem = dynamic_cast<Core::UMLSignalEdge*>(elem);
	Core::UMLClass *classElem = dynamic_cast<Core::UMLClass*>(elem);
	Core::UMLEnumeration *enumElem = dynamic_cast<Core::UMLEnumeration*>(elem);
	Core::UMLGlobalVariables *globalVarsElem = dynamic_cast<Core::UMLGlobalVariables*>(elem);

	QDialog *diag = nullptr;

	if (scriptedNodeElem)
	{
		diag = new EditScriptedNodeElementDialog(scriptedNodeElem, requestingWidget);
	}
	else if (signalEdgeElem)
	{
		EditSignalEdgeDialog *diag_ = new EditSignalEdgeDialog(signalEdgeElem, requestingWidget);
		diag_->setExistingDatatypeNamesList(m_dia->document()->listDatatypeNames());
		diag = diag_;
	}
	else if (classElem)
	{
		EditClassDialog *diag_ = new EditClassDialog(classElem, requestingWidget);
		diag_->setExistingDatatypeNamesList(m_dia->document()->listDatatypeNames());
		diag = diag_;
	}
	else if (enumElem)
	{
		diag = new EditEnumerationDialog(enumElem, requestingWidget);
	}
	else if (globalVarsElem)
	{
		EditGlobalVariablesDialog *diag_ = new EditGlobalVariablesDialog(globalVarsElem, requestingWidget);
		diag_->setExistingDatatypeNamesList(m_dia->document()->listDatatypeNames());
		diag = diag_;
	}
	else
	{
		qFatal("This should never happen");
	}

	connect(diag, SIGNAL(finished(int)), diag, SLOT(deleteLater()));
	diag->show();
}

void UMLGraphicsScene::deleteSelectedItems(QWidget *requestingWidget)
{
	// What elements must be deleted? Note that QSet automatically removes
	// duplicate entries
	QSet<Core::UMLElement*> elementsToBeDeletedSet;

	foreach (UMLElement *item, m_relaxedSelection)
	{
		Core::UMLElement *coreItem = item->coreItem();
		Core::UMLNodeElement *nodeItem = dynamic_cast<Core::UMLNodeElement*>(coreItem);

		// If we are removing a node, remove its edges too
		if (nodeItem != nullptr)
		{
			foreach (Core::UMLEdgeElement *edge,
				nodeItem->incomingEdges() + nodeItem->outgoingEdges())
			{
				elementsToBeDeletedSet.insert(edge);
			}
		}

		elementsToBeDeletedSet.insert(coreItem);
	}

	// Sort elementsToBeDeletedSet according to their type so
	// that references are never broken during the deletion process
	QList<Core::UMLElement*> elementsToBeDeletedList = elementsToBeDeletedSet.toList();
	Core::UMLElement::topoSort(elementsToBeDeletedList, true);

	foreach (Core::UMLElement *elem, elementsToBeDeletedList)
		m_dia->deleteUMLElement(elem);
}

UMLNodeElement *UMLGraphicsScene::searchNodeElementAt(const QPointF &scenePos) const
{
	foreach (QGraphicsItem *qtItem, items(scenePos))
	{
		UMLElement *elem = UMLElement::lookup(qtItem, false);
		if (elem == nullptr)
			continue;

		if (dynamic_cast<Core::UMLNodeElement*>(elem->coreItem()) != nullptr)
			return static_cast<UMLNodeElement*>(elem);
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
	emit fillContextMenu(&menu);
	if (!menu.isEmpty())
		menu.exec(contextMenuEvent->screenPos());
}

void UMLGraphicsScene::dragMoveEvent(QGraphicsSceneDragDropEvent *event)
{
	// Do not accept further drags while a path is being constructed
	if (m_edgeConstructionOrigin != nullptr)
		return;

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
	if (m_edgeConstructionOrigin != nullptr)
	{
		if (m_edgeConstructSignal)
			painter->setPen(Qt::DashDotLine);

		UMLNodeElement *itemUnderMouse = searchNodeElementAt(m_mousePos);

		// paths to self require at least two intermediate points
		if (m_edgeConstructionOrigin == itemUnderMouse
			&& m_edgeConstructionPoints.isEmpty())
		{
			// do not paint any path
		}
		else if (itemUnderMouse == nullptr || (m_edgeConstructionOrigin == itemUnderMouse
			&& m_edgeConstructionPoints.count() == 1))
		{
			QPolygonF path = m_edgeConstructionPoints;
			path.append(m_mousePos);
			path.prepend(m_edgeConstructionOrigin->closestOutlinePoint(path.first()));
			painter->drawPolyline(path);
		}
		else
		{
			painter->drawPolyline(UMLEdgeElement::calcPathBetweenNodes(
				m_edgeConstructionOrigin,
				itemUnderMouse, m_edgeConstructionPoints));
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
			elem->setNodeName("InitialNode");

			UMLInitialNode *item = new UMLInitialNode();
			item->bind(elem);
			item->setPos(scenePos);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "FinalNode")
		{
			Core::UMLFinalNode *elem = new Core::UMLFinalNode();
			elem->setNodeName(m_dia->document()->generateFreshName("FinalNode"));

			UMLFinalNode *item = new UMLFinalNode();
			item->bind(elem);
			item->setPos(scenePos);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "ActionNode")
		{
			Core::UMLActionNode *elem = new Core::UMLActionNode();
			elem->setNodeName(m_dia->document()->generateFreshName("ActionNode"));

			UMLActionNode *item = new UMLActionNode();
			item->bind(elem);
			item->setPos(scenePos);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "DecisionNode" || elementTypeString == "MergeNode")
		{
			Core::UMLDecisionMergeNode *elem = new Core::UMLDecisionMergeNode();
			elem->setNodeName(m_dia->document()->generateFreshName(elementTypeString));

			UMLDecisionMergeNode *item = new UMLDecisionMergeNode();
			item->bind(elem);
			item->setPos(scenePos);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "ForkNode" || elementTypeString == "JoinNode")
		{
			Core::UMLForkJoinNode *elem = new Core::UMLForkJoinNode();
			elem->setNodeName(m_dia->document()->generateFreshName(elementTypeString));

			UMLForkJoinNode *item = new UMLForkJoinNode();
			item->bind(elem);
			item->setPos(scenePos);

			m_dia->addUMLElement(elem);
		}
	}
	else if (m_dia->type() == Core::UMLDiagram::Activity
		&& mime->formats().contains("application/x-uml-create-flow"))
	{
		m_edgeConstructionOrigin = searchNodeElementAt(scenePos);
		m_edgeConstructionPoints.clear();
		m_edgeConstructSignal = mime->data("application/x-uml-create-flow") == "Signal";
		emit edgeConstructionStateChanged(true);
	}
	else if (m_dia->type() == Core::UMLDiagram::Class
		&& mime->formats().contains("application/x-uml-create-datatype"))
	{
		const QByteArray elementTypeString = mime->data("application/x-uml-create-datatype");

		if (elementTypeString == "Class")
		{
			Core::UMLClass *elem = new Core::UMLClass();
			elem->setDatatypeName(m_dia->document()->generateFreshName("Class"));

			UMLClass *item = new UMLClass();
			item->bind(elem);
			item->setPos(scenePos);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "Enumeration")
		{
			Core::UMLEnumeration *elem = new Core::UMLEnumeration();
			elem->setDatatypeName(m_dia->document()->generateFreshName("Enumeration"));

			UMLEnumeration *item = new UMLEnumeration();
			item->bind(elem);
			item->setPos(scenePos);

			m_dia->addUMLElement(elem);
		}
		else if (elementTypeString == "GlobalVariables")
		{
			Core::UMLGlobalVariables *elem = new Core::UMLGlobalVariables();

			UMLGlobalVariables *item = new UMLGlobalVariables();
			item->bind(elem);
			item->setPos(scenePos);

			m_dia->addUMLElement(elem);
		}
	}
}

void UMLGraphicsScene::keyPressEvent(QKeyEvent *keyEvent)
{
	// ESC stops edge construction
	if (m_edgeConstructionOrigin != nullptr && keyEvent->key() == Qt::Key_Escape)
	{
		m_edgeConstructionOrigin = nullptr;
		emit edgeConstructionStateChanged(false);
		emit changed(QList<QRectF>());
		keyEvent->accept();
		return;
	}

	QGraphicsScene::keyPressEvent(keyEvent);
}

void UMLGraphicsScene::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	// ignore double-clicks while an edge is being constructed
	if (m_edgeConstructionOrigin != nullptr)
		return;

	QGraphicsScene::mouseDoubleClickEvent(mouseEvent);

	// if the user double-clicked on an element and it is the only selected
	// one, edit that element
	if (m_relaxedSelection.count() == 1)
	{
		QGraphicsItem *qtItem = itemAt(mouseEvent->scenePos());
		if (qtItem != nullptr)
		{
			UMLElement *itemUnderMouse = UMLElement::lookup(qtItem);
			if (itemUnderMouse == m_relaxedSelection[0])
			{
				if (canBeEdited(itemUnderMouse->coreItem()))
					editSelectedItem(mouseEvent->widget());
				else if (canBeRenamed(itemUnderMouse->coreItem()))
					renameSelectedItem(mouseEvent->widget());
			}
		}
	}
}

void UMLGraphicsScene::mousePressEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	const QPointF scenePos = mouseEvent->scenePos();

	// Right click aborts edge creation
	if (mouseEvent->button() == Qt::RightButton && m_edgeConstructionOrigin != nullptr)
	{
		m_edgeConstructionOrigin = nullptr;
		emit edgeConstructionStateChanged(false);
		emit changed(QList<QRectF>());
	}

	if (m_edgeConstructionOrigin != nullptr)
	{
		UMLNodeElement *itemUnderMouse = searchNodeElementAt(scenePos);

		if (itemUnderMouse != nullptr)
		{
			// paths to self look decent enough if there two
			// intermediate points at least
			if (m_edgeConstructionOrigin == itemUnderMouse &&
				m_edgeConstructionPoints.count() < 2)
			{
				return;
			}

			Core::UMLNodeElement *from =
				static_cast<Core::UMLNodeElement*>(m_edgeConstructionOrigin->coreItem());
			Core::UMLNodeElement *to =
				static_cast<Core::UMLNodeElement*>(itemUnderMouse->coreItem());

			if (m_edgeConstructSignal)
			{
				Core::UMLSignalEdge *elem = new Core::UMLSignalEdge(from, to);
				elem->setSignalName(m_dia->document()->generateFreshName("Signal"));

				UMLSignalEdge *item = new UMLSignalEdge();
				item->bind(elem);

				item->setIntermediatePoints(m_edgeConstructionPoints);

				m_dia->addUMLElement(elem);
			}
			else
			{
				Core::UMLControlFlowEdge *elem = new Core::UMLControlFlowEdge(from, to);
				UMLControlFlowEdge *item = new UMLControlFlowEdge();
				item->bind(elem);

				item->setIntermediatePoints(m_edgeConstructionPoints);

				m_dia->addUMLElement(elem);
			}

			m_edgeConstructionOrigin = nullptr;
			emit edgeConstructionStateChanged(false);
			emit changed(QList<QRectF>());
		}
		else if (m_edgeConstructionPoints.isEmpty()
			|| m_edgeConstructionPoints.last() != scenePos)
		{
			m_edgeConstructionPoints.append(scenePos);
			emit changed(QList<QRectF>());
		}

		return;
	}

	QGraphicsScene::mousePressEvent(mouseEvent);
}

void UMLGraphicsScene::mouseMoveEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	if (m_edgeConstructionOrigin)
	{
		m_mousePos = mouseEvent->scenePos();
		emit changed(QList<QRectF>());
	}

	QGraphicsScene::mouseMoveEvent(mouseEvent);
}

void UMLGraphicsScene::mouseReleaseEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	QGraphicsScene::mouseReleaseEvent(mouseEvent);

	if (mouseEvent->buttons() == 0 && m_edgeConstructionOrigin == nullptr
		&& m_somethingWasMoved)
	{
		scheduleUndoCheckpoint();
	}
}

void UMLGraphicsScene::notifyElementAdded(Core::UMLElement *element)
{
	UMLElement *item = UMLElement::lookup(element);
	item->refresh();
	addItem(item->qtItem());

	scheduleUndoCheckpoint();
	emit sceneRectMayHaveChanged();
}

void UMLGraphicsScene::notifyElementChanged(Core::UMLElement *element)
{
	UMLElement *item = UMLElement::lookup(element);
	item->refresh();

	scheduleUndoCheckpoint();
}

void UMLGraphicsScene::notifyElementRemoved(Core::UMLElement *element)
{
	UMLElement *item = UMLElement::lookup(element);
	removeItem(item->qtItem());
	delete item;

	// If we have just removed the origin of the edge we were constructing,
	// stop constructing the edge
	if (m_edgeConstructionOrigin == item)
		m_edgeConstructionOrigin = nullptr;

	scheduleUndoCheckpoint();
}

void UMLGraphicsScene::storeGuiDataToXml(Core::UMLElement *coreItem, QDomElement &target, QDomDocument &doc) const
{
	UMLElement *elem = UMLElement::lookup(coreItem);
	elem->storeToXml(target, doc);
}

bool UMLGraphicsScene::loadGuiDataFromXml(Core::UMLElement *coreElem, const QDomElement &source)
{
	switch (coreElem->type())
	{
		case Core::UMLElementType::InitialNode:
		{
			UMLInitialNode *gui = new UMLInitialNode();
			gui->bind(static_cast<Core::UMLInitialNode*>(coreElem));
			gui->loadFromXml(source);
			break;
		}
		case Core::UMLElementType::FinalNode:
		{
			UMLFinalNode *gui = new UMLFinalNode();
			gui->bind(static_cast<Core::UMLFinalNode*>(coreElem));
			gui->loadFromXml(source);
			break;
		}
		case Core::UMLElementType::ActionNode:
		{
			UMLActionNode *gui = new UMLActionNode();
			gui->bind(static_cast<Core::UMLActionNode*>(coreElem));
			gui->loadFromXml(source);
			break;
		}
		case Core::UMLElementType::DecisionMergeNode:
		{
			UMLDecisionMergeNode *gui = new UMLDecisionMergeNode();
			gui->bind(static_cast<Core::UMLDecisionMergeNode*>(coreElem));
			gui->loadFromXml(source);
			break;
		}
		case Core::UMLElementType::ForkJoinNode:
		{
			UMLForkJoinNode *gui = new UMLForkJoinNode();
			gui->bind(static_cast<Core::UMLForkJoinNode*>(coreElem));
			gui->loadFromXml(source);
			break;
		}
		case Core::UMLElementType::ControlFlowEdge:
		{
			UMLControlFlowEdge *gui = new UMLControlFlowEdge();
			gui->bind(static_cast<Core::UMLControlFlowEdge*>(coreElem));
			gui->loadFromXml(source);
			break;
		}
		case Core::UMLElementType::SignalEdge:
		{
			UMLSignalEdge *gui = new UMLSignalEdge();
			gui->bind(static_cast<Core::UMLSignalEdge*>(coreElem));
			gui->loadFromXml(source);
			break;
		}
		case Core::UMLElementType::Class:
		{
			UMLClass *gui = new UMLClass();
			gui->bind(static_cast<Core::UMLClass*>(coreElem));
			gui->loadFromXml(source);
			break;
		}
		case Core::UMLElementType::Enumeration:
		{
			UMLEnumeration *gui = new UMLEnumeration();
			gui->bind(static_cast<Core::UMLEnumeration*>(coreElem));
			gui->loadFromXml(source);
			break;
		}
		case Core::UMLElementType::GlobalVariables:
		{
			UMLGlobalVariables *gui = new UMLGlobalVariables();
			gui->bind(static_cast<Core::UMLGlobalVariables*>(coreElem));
			gui->loadFromXml(source);
			break;
		}
	}

	return true;
}

void UMLGraphicsScene::notifyGeometryChanged(UMLElement *element)
{
	Core::UMLElement *changedElement = element->coreItem();
	Core::UMLNodeElement *node = dynamic_cast<Core::UMLNodeElement*>(changedElement);

	// Re-route edges
	if (node != nullptr)
	{
		foreach (Core::UMLEdgeElement *edge,
			node->incomingEdges() + node->outgoingEdges())
		{
			UMLElement::lookup(edge)->refresh();
		}
	}

	m_somethingWasMoved = true;
}

void UMLGraphicsScene::scheduleUndoCheckpoint()
{
	if (m_undoIsAlreadyScheduled
		|| m_dia->document()->isDeserializationInProgress())
	{
		return;
	}

	// schedule a timer event at the end of the event queue
	m_undoIsAlreadyScheduled = true;
	QTimer::singleShot(0, this, SLOT(createUndoCheckpoint()));
}

void UMLGraphicsScene::createUndoCheckpoint()
{
	// reset scheduling logic, so that subsequent events will result in a
	// new undo checkpoint
	Q_ASSERT(m_undoIsAlreadyScheduled);
	m_undoIsAlreadyScheduled = false;
	m_somethingWasMoved = false;

	// create our checkpoint
	emit undoCheckpointCreationRequest();
}

bool UMLGraphicsScene::canBeEdited(Core::UMLElement *element)
{
	switch (element->type())
	{
		case Core::UMLElementType::InitialNode:
		case Core::UMLElementType::FinalNode:
		case Core::UMLElementType::ForkJoinNode:
		case Core::UMLElementType::ControlFlowEdge:
			return false;
		default:
			return true;
	}
}

bool UMLGraphicsScene::canBeRenamed(Core::UMLElement *element)
{
	return true;
}

}
