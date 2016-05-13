#include "Gui/PropertyListEditWidget.h"

#include <QDebug>
#include <QEvent>

#include "ui_PropertyListEditWidget.h"

namespace Gui
{

PropertyListEditWidget::PropertyListEditWidget(QWidget *parent)
: QWidget(parent), m_ui(new Ui_PropertyListEditWidget)
{
	m_ui->setupUi(this);
	m_ui->treeWidget->header()->resizeSections(QHeaderView::ResizeToContents);
	m_ui->treeWidget->installEventFilter(this);
}

PropertyListEditWidget::~PropertyListEditWidget()
{
	m_ui->treeWidget->removeEventFilter(this);
	delete m_ui;
}

bool PropertyListEditWidget::eventFilter(QObject *obj, QEvent *event)
{
	if (obj == m_ui->treeWidget)
	{
		switch (event->type())
		{
			case QEvent::FocusIn:
				emit focusReceived();
				return QWidget::eventFilter(obj, event);
			default:
				break;
		}
	}

	return QWidget::eventFilter(obj, event);
}

void PropertyListEditWidget::slotAdd()
{
	emit focusReceived();
	qDebug() << "Add";
}

void PropertyListEditWidget::editSelectedItem()
{
	emit focusReceived();
	qDebug() << "Edit";
}

void PropertyListEditWidget::renameSelectedItem()
{
	emit focusReceived();
	qDebug() << "Rename";
}

void PropertyListEditWidget::removeSelectedItem()
{
	emit focusReceived();
	qDebug() << "Remove";
}

void PropertyListEditWidget::slotMoveUp()
{
	emit focusReceived();
	qDebug() << "MoveUp";
}

void PropertyListEditWidget::slotMoveDown()
{
	emit focusReceived();
	qDebug() << "MoveDown";
	updateActions();
}

void PropertyListEditWidget::slotCurrentRowChanged()
{
	emit focusReceived();
	qDebug() << "CurrentRowChanged";
}

void PropertyListEditWidget::updateActions()
{
	emit actionsEnabledChanged(true, true, false, false);
}

}
