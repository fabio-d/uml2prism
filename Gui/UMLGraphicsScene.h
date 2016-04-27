#ifndef GUI_UMLGRAPHICSCENE_H
#define GUI_UMLGRAPHICSCENE_H

#include "Core/GuiProxy.h"

#include <QGraphicsScene>
#include <QMap>
#include <QList>

class QAction;
class QMenu;
class QToolBar;

namespace Core
{
class UMLDiagram;
}

namespace Gui
{
class UMLElement;
class UMLNodeElement;

class UMLGraphicsScene : public QGraphicsScene, private Core::GuiProxy
{
	Q_OBJECT

	public:
		explicit UMLGraphicsScene(Core::UMLDiagram *dia, QObject *parent = nullptr);
		~UMLGraphicsScene();

		void notifyGeometryChanged(UMLElement *element);

		void renameSelectedItem(QWidget *requestingWidget);
		void editSelectedItem(QWidget *requestingWidget);
		void deleteSelectedItems(QWidget *requestingWidget);

	signals:
		void sceneRectMayHaveChanged();
		void edgeConstructionStateChanged(bool inProgress);
		void actionsEnabledChanged(bool editEnabled, bool deleteEnabled);
		void fillContextMenu(QMenu *menu);
		void undoCheckpointCreationRequest();

	private slots:
		void slotSelectionChanged();
		void createUndoCheckpoint();

	private:
		UMLNodeElement *searchNodeElementAt(const QPointF &scenePos) const;
		void scheduleUndoCheckpoint();

		void contextMenuEvent(QGraphicsSceneContextMenuEvent *contextMenuEvent) override;
		void dragMoveEvent(QGraphicsSceneDragDropEvent *event) override;
		void drawForeground(QPainter *painter, const QRectF &rect) override;
		void dropEvent(QGraphicsSceneDragDropEvent *event) override;
		void keyPressEvent(QKeyEvent *keyEvent) override;
		void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *mouseEvent) override;
		void mousePressEvent(QGraphicsSceneMouseEvent *mouseEvent) override;
		void mouseMoveEvent(QGraphicsSceneMouseEvent *mouseEvent) override;
		void mouseReleaseEvent(QGraphicsSceneMouseEvent *mouseEvent) override;

		void notifyElementAdded(Core::UMLElement *element) override;
		void notifyElementChanged(Core::UMLElement *element) override;
		void notifyElementRemoved(Core::UMLElement *element) override;
		void storeGuiDataToXml(Core::UMLElement *element, QDomElement &target, QDomDocument &doc) const override;
		bool loadGuiDataFromXml(Core::UMLElement *element, const QDomElement &source) override;

		// The UML diagram
		Core::UMLDiagram *m_dia;

		// Currently selected items. m_strictSelection is a subset of
		// m_relaxedSelection that only contains those items whose
		// main UMLElement is selected.
		QList<UMLElement*> m_strictSelection, m_relaxedSelection;

		// Information used to create graph edges
		UMLNodeElement *m_edgeConstructionOrigin;
		QPolygonF m_edgeConstructionPoints;
		bool m_edgeConstructSignal; // false=control flow, true=signal
		QPointF m_mousePos;

		// Was something moved since the last undo checkpoint?
		bool m_somethingWasMoved;

		// The creation of undo checkpoints is deferred at the end of
		// the event queue, so that multiple related events do not
		// result in multiple calls. This variable keeps track of
		// whether the creation of a checkpoint is already pending or
		// not
		bool m_undoIsAlreadyScheduled;
};

}

#endif // GUI_UMLGRAPHICSCENE_H
