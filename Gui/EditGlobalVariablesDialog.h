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

#ifndef GUI_EDITGLOBALVARIABLESDIALOG_H
#define GUI_EDITGLOBALVARIABLESDIALOG_H

#include <QDialog>

#include "Gui/EditListWidget.h"

class Ui_EditGlobalVariablesDialog;

namespace Core
{
class UMLGlobalVariables;
}

namespace Gui
{

class EditGlobalVariablesDialog : public QDialog, private EditListWidgetCallbacks
{
	Q_OBJECT

	public:
		explicit EditGlobalVariablesDialog(Core::UMLGlobalVariables *globalVariables, QWidget *parent = nullptr);
		~EditGlobalVariablesDialog();

		void setExistingDatatypeNamesList(const QStringList &list);

		void accept() override;

	private slots:
		void slotEdited();

	private:
		void setEditorData(const QVariant &initialData) override;
		void setEditorEnabled(bool enable) override;
		QString formatData(const QVariant &data) override;
		void formatFont(const QVariant &data, QFont &font) override;
		QVariant generateEmptyEntry() override;
		bool testEntryEmpty(const QVariant &data) override;

		Ui_EditGlobalVariablesDialog *m_ui;
		Core::UMLGlobalVariables *m_globalVariables;
		bool m_ignoreEditSignals;
};

}

#endif // GUI_EDITGLOBALVARIABLESDIALOG_H
