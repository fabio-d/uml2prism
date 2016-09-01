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

#ifndef GUI_IDENTIFIERVALIDATOR_H
#define GUI_IDENTIFIERVALIDATOR_H

#include <QRegExpValidator>

namespace Gui
{

class IdentifierValidator : public QRegExpValidator
{
	public:
		IdentifierValidator(QObject *parent = nullptr);
};

class StrictIdentifierValidator : public QRegExpValidator
{
	public:
		StrictIdentifierValidator(QObject *parent = nullptr);

		static bool isAcceptable(const QString &str);
		static bool checkNodeNameWithMessageBox(QWidget *parent, const QString &str);
		static bool checkEnumValueWithMessageBox(QWidget *parent, const QString &str);
		static bool checkDatatypeWithMessageBox(QWidget *parent, const QString &str);
		static bool checkSignalNameWithMessageBox(QWidget *parent, const QString &str);
		static bool checkVariableNameWithMessageBox(QWidget *parent, const QString &str);
};

}

#endif // GUI_IDENTIFIERVALIDATOR_H
