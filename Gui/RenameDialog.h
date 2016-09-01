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

#ifndef GUI_RENAMEDIALOG_H
#define GUI_RENAMEDIALOG_H

#include <QDialog>

class Ui_RenameDialog;

namespace Core
{
class Predicate;
enum class PredicateType;
class UMLControlFlowEdge;
class UMLDatatypeElement;
class UMLElement;
class UMLNodeElement;
class UMLSignalEdge;
}

namespace Gui
{

class RenameDialog : public QDialog
{
	Q_OBJECT

	public:
		RenameDialog(Core::Predicate *pred, Core::PredicateType type, QWidget *parent = nullptr);
		explicit RenameDialog(Core::UMLElement *elem, QWidget *parent = nullptr);
		~RenameDialog();

		void accept() override;

	private slots:
		void slotTextChanged();

	private:
		Ui_RenameDialog *m_ui;
		Core::UMLNodeElement *m_nodeElem;
		Core::UMLControlFlowEdge *m_controlFlowElem;
		Core::UMLSignalEdge *m_signalFlowElem;
		Core::UMLDatatypeElement *m_datatypeElem;
		Core::Predicate *m_pred;
		Core::PredicateType m_predType;
};

}

#endif // GUI_RENAMEDIALOG_H
