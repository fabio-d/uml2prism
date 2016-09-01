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

#include "Gui/EditEnumerationDialog.h"

#include "Gui/IdentifierValidator.h"

#include "Core/UMLElement.h"

#include "ui_EditEnumerationDialog.h"

namespace Gui
{

EditEnumerationDialog::EditEnumerationDialog(Core::UMLEnumeration *enumeration, QWidget *parent)
: QDialog(parent), m_ui(new Ui_EditEnumerationDialog),
  m_enumeration(enumeration)
{
	m_ui->setupUi(this);
	m_ui->editListWidget->setCallbacks(this);

	m_ui->nameLineEdit->setValidator(new IdentifierValidator(this));
	m_ui->valueLineEdit->setValidator(new IdentifierValidator(this));

	setWindowTitle(QString("Enumeration %1").arg(m_enumeration->datatypeName()));

	m_ui->nameLineEdit->setText(m_enumeration->datatypeName());

	QList<QVariant> varValues;
	foreach (const QString &val, m_enumeration->values())
		varValues.append(val);
	m_ui->editListWidget->setValues(varValues);
}

EditEnumerationDialog::~EditEnumerationDialog()
{
	delete m_ui;
}

void EditEnumerationDialog::accept()
{
	if (!StrictIdentifierValidator::checkDatatypeWithMessageBox(this, m_ui->nameLineEdit->text()))
		return;

	QStringList values;
	foreach (const QVariant &val, m_ui->editListWidget->values())
	{
		const QString strVal = val.toString();
		if (!StrictIdentifierValidator::checkEnumValueWithMessageBox(this, strVal))
			return;
		values.append(strVal);
	}

	m_enumeration->setDatatypeName(m_ui->nameLineEdit->text());
	m_enumeration->setValues(values);

	QDialog::accept();
}

void EditEnumerationDialog::slotEdited()
{
	m_ui->editListWidget->setEditedData(m_ui->valueLineEdit->text());
}

void EditEnumerationDialog::setEditorData(const QVariant &initialData)
{
	m_ui->valueLineEdit->setText(initialData.toString());
}

void EditEnumerationDialog::setEditorEnabled(bool enable)
{
	m_ui->valueLineEdit->setEnabled(enable);
}

QString EditEnumerationDialog::formatData(const QVariant &data)
{
	return data.toString();
}

QVariant EditEnumerationDialog::generateEmptyEntry()
{
	return QVariant();
}

bool EditEnumerationDialog::testEntryEmpty(const QVariant &data)
{
	return data.toString().isEmpty();
}

}
