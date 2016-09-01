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

#include "Gui/EditListWidget.h"

#include "ui_EditListWidget.h"

#include <QDebug>

namespace Gui
{

void EditListWidgetCallbacks::formatFont(const QVariant &data, QFont &font)
{
}

EditListWidget::EditListWidget(QWidget *parent)
: QWidget(parent), m_ui(new Ui_EditListWidget), m_callbacks(nullptr)
{
	m_ui->setupUi(this);
}

EditListWidget::~EditListWidget()
{
	delete m_ui;
}

void EditListWidget::setCallbacks(EditListWidgetCallbacks *callbacks)
{
	m_callbacks = callbacks;
	slotCurrentRowChanged();
}

void EditListWidget::setValues(const QList<QVariant> &values)
{
	m_ui->listWidget->clear();

	foreach (const QVariant &val, values)
	{
		QListWidgetItem *item = new QListWidgetItem(m_callbacks->formatData(val));
		item->setData(Qt::UserRole, val);
		updateFontUnderline(item);
		m_ui->listWidget->addItem(item);
	}
}

QList<QVariant> EditListWidget::values() const
{
	QList<QVariant> result;

	for (int i = 0; i < m_ui->listWidget->count(); i++)
	{
		QVariant data = m_ui->listWidget->item(i)->data(Qt::UserRole);
		if (!m_callbacks->testEntryEmpty(data))
			result.append(data);
	}

	return result;
}

void EditListWidget::setEditedData(const QVariant &data)
{
	QListWidgetItem *item = m_ui->listWidget->currentItem();
	Q_ASSERT(item != nullptr);
	item->setText(m_callbacks->formatData(data));
	item->setData(Qt::UserRole, data);
	updateFontUnderline(item);
}

void EditListWidget::slotAdd()
{
	const QVariant val = m_callbacks->generateEmptyEntry();
	QListWidgetItem *item = new QListWidgetItem(m_callbacks->formatData(val));
	item->setData(Qt::UserRole, val);
	updateFontUnderline(item);
	m_ui->listWidget->addItem(item);
	m_ui->listWidget->setCurrentItem(item);
}

void EditListWidget::slotRemove()
{
	const int currentRow = m_ui->listWidget->currentRow();
	Q_ASSERT(currentRow >= 0);
	delete m_ui->listWidget->takeItem(currentRow);
}

void EditListWidget::slotMoveUp()
{
	const int currentRow = m_ui->listWidget->currentRow();
	Q_ASSERT(currentRow > 0);
	m_ui->listWidget->insertItem(currentRow - 1, m_ui->listWidget->takeItem(currentRow));
	m_ui->listWidget->setCurrentRow(currentRow - 1);
}

void EditListWidget::slotMoveDown()
{
	const int currentRow = m_ui->listWidget->currentRow();
	Q_ASSERT(currentRow >= 0 && currentRow != m_ui->listWidget->count() - 1);
	m_ui->listWidget->insertItem(currentRow, m_ui->listWidget->takeItem(currentRow + 1));
	m_ui->listWidget->setCurrentRow(currentRow + 1);

	slotCurrentRowChanged();
	m_ui->listWidget->scrollToItem(m_ui->listWidget->currentItem());
}

void EditListWidget::slotCurrentRowChanged()
{
	const int newRow = m_ui->listWidget->currentRow();

	if (newRow == -1)
	{
		m_callbacks->setEditorEnabled(false);
		m_callbacks->setEditorData(m_callbacks->generateEmptyEntry());
		m_ui->removePushButton->setEnabled(false);
		m_ui->moveUpPushButton->setEnabled(false);
		m_ui->moveDownPushButton->setEnabled(false);
	}
	else
	{
		m_callbacks->setEditorEnabled(true);
		m_callbacks->setEditorData(m_ui->listWidget->currentItem()->data(Qt::UserRole));
		m_ui->removePushButton->setEnabled(true);
		m_ui->moveUpPushButton->setEnabled(newRow != 0);
		m_ui->moveDownPushButton->setEnabled(newRow != m_ui->listWidget->count() - 1);
	}
}

void EditListWidget::updateFontUnderline(QListWidgetItem *item)
{
	QFont font(item->font());
	m_callbacks->formatFont(item->data(Qt::UserRole), font);
	item->setFont(font);
}

}
