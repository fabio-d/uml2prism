#ifndef GUI_UMLDIAGRAMVIEW_H
#define GUI_UMLDIAGRAMVIEW_H

#include "Core/GuiProxy.h"

#include <QGraphicsView>

namespace Gui
{
class UMLGraphicsScene;

class UMLDiagramView : public QGraphicsView
{
	Q_OBJECT

	public:
		explicit UMLDiagramView(QWidget *parent = nullptr);

		void appendViewActions(QWidget *target);

		void setScene(UMLGraphicsScene *scene);

		void setScale(qreal newScale);
		void zoomIn();
		void zoomOut();
		void zoomOriginal();
		void zoomFit();

	private slots:
		void updateScene(const QList<QRectF> &rect);
		void slotEdgeConstructionStateChanged(bool inProgress);

	private:
		void dropEvent(QDropEvent *event) override;
		void mousePressEvent(QMouseEvent *event) override;
		void mouseReleaseEvent(QMouseEvent *event) override;
		void resizeEvent(QResizeEvent *event) override;

		qreal currentScale() const;

		UMLGraphicsScene *m_scene;

		// Some actions implemented by this class
		QAction *m_actionZoomIn;
		QAction *m_actionZoomOut;
		QAction *m_actionZoomOriginal;
		QAction *m_actionZoomFit;

		// is the user constructing an edge?
		bool m_edgeConstructionInProgress;
};

}

#endif // GUI_UMLDIAGRAMVIEW_H
