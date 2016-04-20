#ifndef GUI_UMLGRAPHICSCENE_H
#define GUI_UMLGRAPHICSCENE_H

#include <QGraphicsScene>

namespace Gui
{

class UMLGraphicsScene : public QGraphicsScene
{
	Q_OBJECT

	public:
		explicit UMLGraphicsScene(QObject *parent = nullptr);

		void dragMoveEvent(QGraphicsSceneDragDropEvent *event) override;
		void dropEvent(QGraphicsSceneDragDropEvent *event) override;
};

}

#endif // GUI_UMLGRAPHICSCENE_H
