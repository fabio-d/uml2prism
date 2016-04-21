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

class UMLGraphicsScene : public QGraphicsScene, private Core::GuiProxy
{
	Q_OBJECT

	public:
		explicit UMLGraphicsScene(Core::UMLDocument *doc, QObject *parent = nullptr);
		~UMLGraphicsScene();

		void addActions(QMenu *target);
		void addActions(QToolBar *target);

	private slots:
		void slotSelectionChanged();
		void slotRenameNode();
		void slotDeleteSelection();
		void slotElementChanged();

	private:
		void contextMenuEvent(QGraphicsSceneContextMenuEvent *contextMenuEvent) override;
		void dragMoveEvent(QGraphicsSceneDragDropEvent *event) override;
		void dropEvent(QGraphicsSceneDragDropEvent *event) override;

		void notifyElementAdded(Core::UMLElement *element) override;
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
};

}

#endif // GUI_UMLGRAPHICSCENE_H
