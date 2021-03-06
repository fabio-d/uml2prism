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

#include "Gui/UMLDiagramView.h"

#include "Gui/UMLGraphicsScene.h"

#include <QFontMetrics>
#include <QMouseEvent>

namespace Gui
{

UMLDiagramView::UMLDiagramView(QWidget *parent)
: QGraphicsView(parent), m_scene(nullptr), m_edgeConstructionInProgress(false)
{
}

void UMLDiagramView::setScene(UMLGraphicsScene *scene)
{
	m_scene = scene;
	QGraphicsView::setScene(scene);

	connect(m_scene, SIGNAL(edgeConstructionStateChanged(bool)),
		this, SLOT(slotEdgeConstructionStateChanged(bool)));
	connect(m_scene, SIGNAL(sceneRectMayHaveChanged()),
		this, SLOT(updateScene()));

	m_scene->setSceneRect(m_scene->itemsBoundingRect());
	updateScene(QList<QRectF>());
}

void UMLDiagramView::setScale(qreal newScale)
{
	qreal ratio = newScale / currentScale();
	scale(ratio, ratio);

	if (m_scene != nullptr)
	{
		m_scene->setSceneRect(m_scene->itemsBoundingRect());
		updateScene(QList<QRectF>());
	}
}

qreal UMLDiagramView::currentScale() const
{
	// This ratio is an attempt to counteract DPI-dependent zoom scale. It
	// does not work perfectly, because the actual screen size is slightly
	// different on my two laptops.
	static const qreal dpiRatio = 31 / QFontMetrics(font()).height();

	const QTransform t = transform();
	return t.m11() * dpiRatio;
}

void UMLDiagramView::zoomIn()
{
	setScale(currentScale() * 1.1);
}

void UMLDiagramView::zoomOut()
{
	setScale(currentScale() / 1.1);
}

void UMLDiagramView::zoomOriginal()
{
	setScale(1);
}

void UMLDiagramView::zoomFit()
{
	fitInView(m_scene->itemsBoundingRect(), Qt::KeepAspectRatio);
	setScale(currentScale());
}

void UMLDiagramView::zoomClampedFit()
{
	fitInView(m_scene->itemsBoundingRect(), Qt::KeepAspectRatio);
	setScale(qMin<qreal>(currentScale(), 1));
}

void UMLDiagramView::dropEvent(QDropEvent *event)
{
	// Just give us focus...
	setFocus(Qt::MouseFocusReason);

	// ...and let the scene do the actual work
	QGraphicsView::dropEvent(event);
}

void UMLDiagramView::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::MidButton)
	{
		QMouseEvent fakeEvent(
			event->type(),
			event->pos(),
			event->globalPos(),
			Qt::LeftButton,
			(event->buttons() & ~Qt::MidButton) | Qt::LeftButton,
			event->modifiers());
		setDragMode(QGraphicsView::ScrollHandDrag);
		setInteractive(false);
		QGraphicsView::mousePressEvent(&fakeEvent);
		setInteractive(true);
		event->setAccepted(fakeEvent.isAccepted());
	}
	else
	{
		QGraphicsView::mousePressEvent(event);
	}
}

void UMLDiagramView::mouseReleaseEvent(QMouseEvent *event)
{
	if (event->button() == Qt::MidButton)
	{
		QMouseEvent fakeEvent(
			event->type(),
			event->pos(),
			event->globalPos(),
			Qt::LeftButton,
			(event->buttons() & ~Qt::MidButton) | Qt::LeftButton,
			event->modifiers());
		setInteractive(false);
		QGraphicsView::mouseReleaseEvent(&fakeEvent);
		setInteractive(true);
		event->setAccepted(fakeEvent.isAccepted());
	}
	else
	{
		QGraphicsView::mouseReleaseEvent(event);
	}

	if (m_edgeConstructionInProgress)
		setDragMode(QGraphicsView::NoDrag);
	else
		setDragMode(QGraphicsView::RubberBandDrag);
}

void UMLDiagramView::resizeEvent(QResizeEvent *event)
{
	QGraphicsView::resizeEvent(event);

	if (m_scene != nullptr)
	{
		m_scene->setSceneRect(m_scene->itemsBoundingRect());
		updateScene(QList<QRectF>());
	}
}

void UMLDiagramView::updateScene(const QList<QRectF> &)
{
	Q_ASSERT(m_scene != nullptr);

	QRectF sceneCRect = m_scene->sceneRect();
	QRectF itemsBRect = m_scene->itemsBoundingRect();

	QRectF minimumRect(mapToScene(0, 0),
		mapToScene(viewport()->width(), viewport()->height()));

	// compensate for what seems to be floating point errors in
	// QGraphicsView's code
	const qreal adj = 1 / currentScale();
	minimumRect.adjust(-adj, -adj, adj, adj);

	if (!itemsBRect.isEmpty())
	{
		if (!sceneCRect.contains(itemsBRect))
			m_scene->setSceneRect(sceneCRect |= itemsBRect);

		if (!sceneCRect.contains(minimumRect))
			m_scene->setSceneRect(sceneCRect |= minimumRect);
	}
	else if (sceneCRect != minimumRect)
	{
		m_scene->setSceneRect(minimumRect);
	}

	viewport()->update();
}

void UMLDiagramView::slotEdgeConstructionStateChanged(bool inProgress)
{
	m_edgeConstructionInProgress = inProgress;

	if (m_edgeConstructionInProgress)
		setDragMode(QGraphicsView::NoDrag);
	else
		setDragMode(QGraphicsView::RubberBandDrag);
}

}
