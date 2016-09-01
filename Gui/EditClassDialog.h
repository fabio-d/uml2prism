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

#ifndef GUI_EDITCLASSDIALOG_H
#define GUI_EDITCLASSDIALOG_H

#include <QDialog>

#include "Gui/EditListWidget.h"

class Ui_EditClassDialog;

namespace Core
{
class UMLClass;
}

namespace Gui
{

class EditClassDialog : public QDialog, private EditListWidgetCallbacks
{
	Q_OBJECT

	public:
		explicit EditClassDialog(Core::UMLClass *class_, QWidget *parent = nullptr);
		~EditClassDialog();

		void setExistingDatatypeNamesList(const QStringList &list);

		void accept() override;

	private slots:
		void slotEdited();

	private:
		void setEditorData(const QVariant &initialData) override;
		void setEditorEnabled(bool enable) override;
		QString formatData(const QVariant &data) override;
		QVariant generateEmptyEntry() override;
		bool testEntryEmpty(const QVariant &data) override;

		Ui_EditClassDialog *m_ui;
		Core::UMLClass *m_class;
};

}

#endif // GUI_EDITCLASSDIALOG_H
