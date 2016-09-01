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

#include "Gui/EditDatatypeNameWidget.h"

#include "Gui/IdentifierValidator.h"

#include "ui_EditDatatypeNameWidget.h"

namespace Gui
{

EditDatatypeNameWidget::EditDatatypeNameWidget(QWidget *parent)
: QWidget(parent), m_ui(new Ui_EditDatatypeNameWidget), m_ignoreEditSignals(false)
{
	m_ui->setupUi(this);

	m_ui->otherComboBox->setValidator(new IdentifierValidator(this));
}

EditDatatypeNameWidget::~EditDatatypeNameWidget()
{
	delete m_ui;
}

void EditDatatypeNameWidget::setExistingDatatypeNamesList(const QStringList &list_)
{
	m_ignoreEditSignals = true;

	const QString oldValue = m_ui->otherComboBox->currentText();
	QStringList list(list_);
	list.sort();
	list.removeDuplicates();
	m_ui->otherComboBox->addItems(list);
	selectOrSetText(oldValue);

	m_ignoreEditSignals = false;
}

void EditDatatypeNameWidget::setDatatypeName(const Core::DatatypeName &dt)
{
	m_ignoreEditSignals = true;

	const bool isSet = dt.type() == Core::DatatypeName::Set;
	const Core::DatatypeName *datatype = isSet ? dt.innerDatatype() : &dt;

	m_ui->setCheckBox->setChecked(isSet);
	m_ui->otherComboBox->setEditText(QString());
	m_ui->otherComboBox->setEnabled(false);

	switch (datatype->type())
	{
		case Core::DatatypeName::Bool:
			m_ui->boolRadioButton->setChecked(true);
			break;
		case Core::DatatypeName::Other:
			m_ui->otherRadioButton->setChecked(true);
			m_ui->otherComboBox->setEnabled(true);
			selectOrSetText(datatype->datatypeName());
			break;
		case Core::DatatypeName::Set:
			qFatal("Nested sets are not supported by EditDatatypeNameWidget");
			break;
		default:
			qFatal("This should never happen");
			break;
	}

	m_ignoreEditSignals = false;
}

Core::DatatypeName EditDatatypeNameWidget::datatypeName() const
{
	const Core::DatatypeName res =
		m_ui->boolRadioButton->isChecked() ? Core::DatatypeName::makeBool() :
		Core::DatatypeName::makeOther(m_ui->otherComboBox->currentText());
	return m_ui->setCheckBox->isChecked() ? Core::DatatypeName::makeSet(res) : res;
}

void EditDatatypeNameWidget::slotSomethingChanged()
{
	if (m_ignoreEditSignals == false)
		emit datatypeEdited();
}

void EditDatatypeNameWidget::slotOtherRadioButtonToggled(bool checked)
{
	m_ui->otherComboBox->setEnabled(checked);
}

void EditDatatypeNameWidget::selectOrSetText(const QString &value)
{
	int valueIdx = m_ui->otherComboBox->findText(value);
	if (valueIdx != -1)
		m_ui->otherComboBox->setCurrentIndex(valueIdx);
	else
	{
		m_ui->otherComboBox->setCurrentIndex(-1);
		m_ui->otherComboBox->setEditText(value);
	}
}

}
