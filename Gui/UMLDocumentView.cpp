#include "Gui/UMLDocumentView.h"

#include "Gui/UMLGraphicsScene.h"

#include <QDebug>
#include <QMouseEvent>

namespace Gui
{

UMLDocumentView::UMLDocumentView(QWidget *parent)
: QGraphicsView(parent), m_scene(nullptr)
{
}

void UMLDocumentView::setScene(UMLGraphicsScene *scene)
{
	m_scene = scene;
	QGraphicsView::setScene(scene);

	m_scene->setSceneRect(m_scene->itemsBoundingRect());
	updateScene(QList<QRectF>());
}

void UMLDocumentView::setScale(qreal newScale)
{
	qreal ratio = newScale / currentScale();
	scale(ratio, ratio);

	if (m_scene != nullptr)
	{
		m_scene->setSceneRect(m_scene->itemsBoundingRect());
		updateScene(QList<QRectF>());
	}
}

qreal UMLDocumentView::currentScale() const
{
	const QTransform t = transform();
	return t.m11();
}

void UMLDocumentView::zoomIn()
{
	setScale(currentScale() * 1.1);
}

void UMLDocumentView::zoomOut()
{
	setScale(currentScale() / 1.1);
}

void UMLDocumentView::zoomOriginal()
{
	setScale(1);
}

void UMLDocumentView::zoomFit()
{
	fitInView(m_scene->itemsBoundingRect(), Qt::KeepAspectRatio);
	setScale(currentScale());
}

void UMLDocumentView::mousePressEvent(QMouseEvent *event)
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
		QGraphicsView::mousePressEvent(&fakeEvent);
		event->setAccepted(fakeEvent.isAccepted());
	}
	else
	{
		QGraphicsView::mousePressEvent(event);
	}
}

void UMLDocumentView::mouseReleaseEvent(QMouseEvent *event)
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
		QGraphicsView::mouseReleaseEvent(&fakeEvent);
		event->setAccepted(fakeEvent.isAccepted());
	}
	else
	{
		QGraphicsView::mouseReleaseEvent(event);
	}

	setDragMode(QGraphicsView::RubberBandDrag);
}

void UMLDocumentView::resizeEvent(QResizeEvent *event)
{
	QGraphicsView::resizeEvent(event);

	if (m_scene != nullptr)
	{
		m_scene->setSceneRect(m_scene->itemsBoundingRect());
		updateScene(QList<QRectF>());
	}
}

void UMLDocumentView::updateScene(const QList<QRectF> &)
{
	Q_ASSERT(m_scene != nullptr);

	QRectF sceneCRect = m_scene->sceneRect();
	QRectF itemsBRect = m_scene->itemsBoundingRect();

	QRectF minimumRect(mapToScene(0, 0),
		mapToScene(viewport()->width(), viewport()->height()));

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

}
