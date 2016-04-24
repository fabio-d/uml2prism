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

		void renameSelectedItem();
		void editSelectedItem();
		void deleteSelectedItems();

	signals:
		void edgeConstructionStateChanged(bool inProgress);
		void actionsEnabledChanged(bool editEnabled, bool deleteEnabled);
		void fillContextMenu(QMenu *menu);

	private slots:
		void slotSelectionChanged();

	private:
		UMLNodeElement *searchNodeElementAt(const QPointF &scenePos) const;

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
};

}

#endif // GUI_UMLGRAPHICSCENE_H
