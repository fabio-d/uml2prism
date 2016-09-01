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

#ifndef GUI_EDITSCRIPTEDNODEELEMENTDIALOG_H
#define GUI_EDITSCRIPTEDNODEELEMENTDIALOG_H

#include <QDialog>

class Ui_EditScriptedNodeElementDialog;

namespace Core
{
class Document;
class UMLScriptedNodeElement;
}

namespace Gui
{

class EditScriptedNodeElementDialog : public QDialog
{
	Q_OBJECT

	public:
		EditScriptedNodeElementDialog(Core::Document *doc, Core::UMLScriptedNodeElement *elem, QWidget *parent = nullptr);
		~EditScriptedNodeElementDialog();

		void accept() override;

	private:
		Ui_EditScriptedNodeElementDialog *m_ui;
		Core::Document *m_doc;
		Core::UMLScriptedNodeElement *m_elem;
};

}

#endif // GUI_EDITSCRIPTEDNODEELEMENTDIALOG_H
