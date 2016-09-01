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

#ifndef GUI_PREDICATELISTEDITWIDGET_H
#define GUI_PREDICATELISTEDITWIDGET_H

#include <QWidget>

#include "Core/Document.h"

class QTreeWidgetItem;

class Ui_PredicateListEditWidget;

namespace Core
{
class PredicateList;
}

namespace Gui
{
class UndoManager;

class PredicateListEditWidget : public QWidget
{
	Q_OBJECT

	friend class PredicateListAddUndoCommand;
	friend class PredicateListEditUndoCommand;
	friend class PredicateListRemoveUndoCommand;
	friend class PredicateListMoveUpUndoCommand;
	friend class PredicateListMoveDownUndoCommand;

	public:
		explicit PredicateListEditWidget(QWidget *parent = nullptr);
		~PredicateListEditWidget();

		void setList(Core::PredicateList *list);
		void setUndoManager(UndoManager *undoManager);

	signals:
		void actionsEnabledChanged(bool editEnabled, bool renameEnabled, bool deleteEnabled, bool resetLabelPosEnabled);
		void focusReceived();

	public slots:
		void editSelectedItem();
		void renameSelectedItem();
		void removeSelectedItem();

	private slots:
		void slotAdd();
		void slotMoveUp();
		void slotMoveDown();
		void slotCurrentRowChanged();
		void slotDeserializationCompleted(Core::Document::SerializationOptions loadedWhat);
		void slotItemActivated(QTreeWidgetItem *item);

	private:
		bool eventFilter(QObject *obj, QEvent *event) override;

		void flushChanges();

		Ui_PredicateListEditWidget *m_ui;
		Core::PredicateList *m_docList;
		UndoManager *m_undoManager;
};

}

#endif // GUI_PREDICATELISTEDITWIDGET_H
