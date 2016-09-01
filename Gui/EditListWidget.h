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

#ifndef GUI_EDITLISTWIDGET_H
#define GUI_EDITLISTWIDGET_H

#include <QWidget>

class QListWidgetItem;

class Ui_EditListWidget;

namespace Gui
{

class EditListWidgetCallbacks
{
	public:
		virtual void setEditorData(const QVariant &initialData) = 0;
		virtual void setEditorEnabled(bool enable) = 0;

		virtual QString formatData(const QVariant &data) = 0;
		virtual void formatFont(const QVariant &data, QFont &font);

		virtual QVariant generateEmptyEntry() = 0;
		virtual bool testEntryEmpty(const QVariant &data) = 0;
};

class EditListWidget : public QWidget
{
	Q_OBJECT

	public:
		explicit EditListWidget(QWidget *parent = nullptr);
		~EditListWidget();

		void setCallbacks(EditListWidgetCallbacks *callbacks);

		void setValues(const QList<QVariant> &values);
		QList<QVariant> values() const;

		void setEditedData(const QVariant &data);

	private slots:
		void slotAdd();
		void slotRemove();
		void slotMoveUp();
		void slotMoveDown();
		void slotCurrentRowChanged();

	private:
		void updateFontUnderline(QListWidgetItem *item);

		Ui_EditListWidget *m_ui;
		EditListWidgetCallbacks *m_callbacks;
};

}

#endif // GUI_EDITLISTWIDGET_H
