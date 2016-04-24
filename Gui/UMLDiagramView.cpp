#include "Gui/UMLDiagramView.h"

#include "Gui/UMLGraphicsScene.h"

#include <QAction>
#include <QDebug>
#include <QMouseEvent>
#include <QIcon>

namespace Gui
{

UMLDiagramView::UMLDiagramView(QWidget *parent)
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

void UMLDiagramView::appendViewActions(QWidget *target)
{
	target->addAction(m_actionZoomIn);
	target->addAction(m_actionZoomOut);
	target->addAction(m_actionZoomOriginal);
	target->addAction(m_actionZoomFit);
}

void UMLDiagramView::setScene(UMLGraphicsScene *scene)
{
	m_scene = scene;
	QGraphicsView::setScene(scene);

	connect(m_scene, SIGNAL(edgeConstructionStateChanged(bool)),
		this, SLOT(slotEdgeConstructionStateChanged(bool)));

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
	const QTransform t = transform();
	return t.m11();
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
