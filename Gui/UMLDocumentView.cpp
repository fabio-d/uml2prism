#include "Gui/UMLDocumentView.h"

#include "Gui/UMLGraphicsScene.h"

#include <QAction>
#include <QDebug>
#include <QMouseEvent>
#include <QIcon>

namespace Gui
{

UMLDocumentView::UMLDocumentView(QWidget *parent)
: QGraphicsView(parent), m_scene(nullptr)
{
	m_actionZoomIn = new QAction(
		QIcon(":/kde_icons/resources/kde_icons/zoom-in.png"),
		"Zoom In", this);
	m_actionZoomIn->setShortcut(QKeySequence::ZoomIn);
	connect(m_actionZoomIn, SIGNAL(triggered()), this, SLOT(zoomIn()));

	m_actionZoomOut = new QAction(
		QIcon(":/kde_icons/resources/kde_icons/zoom-out.png"),
		"Zoom Out", this);
	m_actionZoomOut->setShortcut(QKeySequence::ZoomOut);
	connect(m_actionZoomOut, SIGNAL(triggered()), this, SLOT(zoomOut()));

	m_actionZoomOriginal = new QAction(
		QIcon(":/kde_icons/resources/kde_icons/zoom-original.png"),
		"Zoom 100%", this);
	m_actionZoomOriginal->setShortcut(Qt::CTRL + Qt::Key_0);
	connect(m_actionZoomOriginal, SIGNAL(triggered()), this, SLOT(zoomOriginal()));

	m_actionZoomFit = new QAction(
		QIcon(":/kde_icons/resources/kde_icons/zoom-fit-best.png"),
		"Zoom Fit", this);
	m_actionZoomFit->setShortcut(Qt::Key_F2);
	connect(m_actionZoomFit, SIGNAL(triggered()), this, SLOT(zoomFit()));
}

void UMLDocumentView::appendViewActions(QWidget *target)
{
	target->addAction(m_actionZoomIn);
	target->addAction(m_actionZoomOut);
	target->addAction(m_actionZoomOriginal);
	target->addAction(m_actionZoomFit);
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
		setInteractive(false);
		QGraphicsView::mouseReleaseEvent(&fakeEvent);
		setInteractive(true);
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
