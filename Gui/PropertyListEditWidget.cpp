#include "Gui/PropertyListEditWidget.h"

#include <QDebug>

#include "ui_PropertyListEditWidget.h"

namespace Gui
{

PropertyListEditWidget::PropertyListEditWidget(QWidget *parent)
: QWidget(parent), m_ui(new Ui_PropertyListEditWidget)
{
	m_ui->setupUi(this);
}

PropertyListEditWidget::~PropertyListEditWidget()
{
	delete m_ui;
}

void PropertyListEditWidget::slotAdd()
{
	qDebug() << "Add";
}

void PropertyListEditWidget::editSelectedItem()
{
	qDebug() << "Edit";
}

void PropertyListEditWidget::removeSelectedItem()
{
	qDebug() << "Remove";
}

void PropertyListEditWidget::slotMoveUp()
{
	qDebug() << "MoveUp";
}

void PropertyListEditWidget::slotMoveDown()
{
	qDebug() << "MoveDown";
}

void PropertyListEditWidget::slotCurrentRowChanged()
{
	qDebug() << "CurrentRowChanged";
}

}
