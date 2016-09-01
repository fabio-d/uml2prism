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

#ifndef GUI_EDITPREDICATEDIALOG_H
#define GUI_EDITPREDICATEDIALOG_H

#include <QDialog>

class Ui_EditPredicateDialog;

namespace Core
{
class Document;
class Predicate;
enum class PredicateType;
}

namespace Gui
{

class EditPredicateDialog : public QDialog
{
	Q_OBJECT

	public:
		EditPredicateDialog(Core::Document *doc, Core::Predicate *pred, Core::PredicateType type, QWidget *parent = nullptr);
		~EditPredicateDialog();

		void accept() override;

	private:
		Ui_EditPredicateDialog *m_ui;
		Core::Document *m_doc;
		Core::Predicate *m_pred;
		Core::PredicateType m_predType;
};

}

#endif // GUI_EDITPREDICATEDIALOG_H
