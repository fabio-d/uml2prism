#ifndef GUI_UMLDOCUMENTVIEW_H
#define GUI_UMLDOCUMENTVIEW_H

#include "Core/GuiProxy.h"

#include <QGraphicsView>

namespace Gui
{
class UMLGraphicsScene;

class UMLDocumentView : public QGraphicsView
{
	Q_OBJECT

	public:
		explicit UMLDocumentView(QWidget *parent = nullptr);

		void setScene(UMLGraphicsScene *scene);

		void setScale(qreal newScale);

	public slots:
		void zoomIn();
		void zoomOut();
		void zoomOriginal();
		void zoomFit();

	private slots:
		void updateScene(const QList<QRectF> &rect);

	private:
		void mousePressEvent(QMouseEvent *event) override;
		void mouseReleaseEvent(QMouseEvent *event) override;
		void resizeEvent(QResizeEvent *event) override;

		qreal currentScale() const;

		UMLGraphicsScene *m_scene;
};

}

#endif // GUI_UMLDOCUMENTVIEW_H
