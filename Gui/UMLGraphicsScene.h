#ifndef GUI_UMLGRAPHICSCENE_H
#define GUI_UMLGRAPHICSCENE_H

#include "Core/GuiProxy.h"

#include <QGraphicsScene>
#include <QMap>

namespace Core
{
class UMLDocument;
}

namespace Gui
{
class UMLGraphicsItem;

class UMLGraphicsScene : public QGraphicsScene, private Core::GuiProxy
{
	Q_OBJECT

	public:
		explicit UMLGraphicsScene(Core::UMLDocument *doc, QObject *parent = nullptr);
		~UMLGraphicsScene();

	private:
		void dragMoveEvent(QGraphicsSceneDragDropEvent *event) override;
		void dropEvent(QGraphicsSceneDragDropEvent *event) override;
		void keyPressEvent(QKeyEvent *keyEvent) override;

		void notifyElementAdded(Core::UMLElement *element) override;
		void notifyElementChanged(Core::UMLElement *element) override;
		void notifyElementRemoved(Core::UMLElement *element) override;

		Core::UMLDocument *m_doc;
};

}

#endif // GUI_UMLGRAPHICSCENE_H
