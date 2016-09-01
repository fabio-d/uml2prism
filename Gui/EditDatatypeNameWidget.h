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

#ifndef GUI_EDITDATATYPENAMEWIDGET_H
#define GUI_EDITDATATYPENAMEWIDGET_H

#include "Core/DatatypeName.h"

#include <QWidget>

class Ui_EditDatatypeNameWidget;

namespace Gui
{

class EditDatatypeNameWidget : public QWidget
{
	Q_OBJECT

	public:
		explicit EditDatatypeNameWidget(QWidget *parent = nullptr);
		~EditDatatypeNameWidget();

		void setExistingDatatypeNamesList(const QStringList &list);

		void setDatatypeName(const Core::DatatypeName &dt);
		Core::DatatypeName datatypeName() const;

	signals:
		void datatypeEdited();

	private slots:
		void slotSomethingChanged();
		void slotOtherRadioButtonToggled(bool checked);

	private:
		void selectOrSetText(const QString &value);

		Ui_EditDatatypeNameWidget *m_ui;
		bool m_ignoreEditSignals;
};

}

#endif // GUI_EDITDATATYPENAMEWIDGET_H
