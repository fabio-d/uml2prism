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

#ifndef GUI_MODELPREVIEWDIALOG_H
#define GUI_MODELPREVIEWDIALOG_H

#include <QDialog>

class Ui_ModelPreviewDialog;

namespace Gui
{

class ModelPreviewDialog : public QDialog
{
	Q_OBJECT

	public:
		ModelPreviewDialog(const QString &model, const QString &propertiesPCTL, const QString &propertiesCTL, QWidget *parent = nullptr);
		~ModelPreviewDialog();

	private slots:
		void propTypeChanged();

	private:
		Ui_ModelPreviewDialog *m_ui;
		QString m_nm, m_pctl, m_ctl;
};

}

#endif // GUI_MODELPREVIEWDIALOG_H
