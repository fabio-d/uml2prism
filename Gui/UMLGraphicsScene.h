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
class UMLDocument;
}

namespace Gui
{
class UMLElement;
class UMLNodeElement;

class UMLGraphicsScene : public QGraphicsScene, private Core::GuiProxy
{
	Q_OBJECT

	public:
		explicit UMLGraphicsScene(Core::UMLDocument *doc, QObject *parent = nullptr);
		~UMLGraphicsScene();

		void appendEditActions(QWidget *target);

		void notifyGeometryChanged(UMLElement *element);

	private slots:
		void slotSelectionChanged();
		void slotRenameNode();
		void slotDeleteSelection();

	private:
		UMLNodeElement *searchNodeElementAt(const QPointF &scenePos) const;

		void contextMenuEvent(QGraphicsSceneContextMenuEvent *contextMenuEvent) override;
		void dragMoveEvent(QGraphicsSceneDragDropEvent *event) override;
		void drawForeground(QPainter *painter, const QRectF &rect) override;
		void dropEvent(QGraphicsSceneDragDropEvent *event) override;
		void mousePressEvent(QGraphicsSceneMouseEvent *mouseEvent) override;
		void mouseMoveEvent(QGraphicsSceneMouseEvent *mouseEvent) override;
		void mouseReleaseEvent(QGraphicsSceneMouseEvent *mouseEvent) override;

		void notifyElementAdded(Core::UMLElement *element) override;
		void notifyElementChanged(Core::UMLElement *element) override;
		void notifyElementRemoved(Core::UMLElement *element) override;

		// The UML document
		Core::UMLDocument *m_doc;

		// Some actions implemented by this class
		QAction *m_actionRenameNode;
		QAction *m_actionDeleteSelection;

		// Currently selected items. m_strictSelection is a subset of
		// m_relaxedSelection that only contains those items whose
		// main UMLElement is selected.
		QList<UMLElement*> m_strictSelection, m_relaxedSelection;

		// When creating a flow element, this is the origin node
		UMLNodeElement *m_createFlowOrigin;
		QPolygonF m_intermediatePoints;
		QPointF m_mousePos;
};

}

#endif // GUI_UMLGRAPHICSCENE_H
