/*
 * Copyright (C) 2016 Fabio D'Urso
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

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

		void setScene(UMLGraphicsScene *scene);

		void setScale(qreal newScale);
		void zoomIn();
		void zoomOut();
		void zoomOriginal();
		void zoomFit();
		void zoomClampedFit();

	private slots:
		void updateScene(const QList<QRectF> &rect = QList<QRectF>());
		void slotEdgeConstructionStateChanged(bool inProgress);

	private:
		void dropEvent(QDropEvent *event) override;
		void mousePressEvent(QMouseEvent *event) override;
		void mouseReleaseEvent(QMouseEvent *event) override;
		void resizeEvent(QResizeEvent *event) override;

		qreal currentScale() const;

		UMLGraphicsScene *m_scene;

		// is the user constructing an edge?
		bool m_edgeConstructionInProgress;
};

}

#endif // GUI_UMLDIAGRAMVIEW_H
