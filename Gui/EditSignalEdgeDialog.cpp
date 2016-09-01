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

#include "Gui/EditSignalEdgeDialog.h"

#include "Gui/IdentifierValidator.h"

#include "Core/UMLElement.h"

#include "ui_EditSignalEdgeDialog.h"

namespace Gui
{

EditSignalEdgeDialog::EditSignalEdgeDialog(Core::UMLSignalEdge *signalEdge, QWidget *parent)
: QDialog(parent), m_ui(new Ui_EditSignalEdgeDialog),
  m_signalEdge(signalEdge)
{
	m_ui->setupUi(this);

	m_ui->nameLineEdit->setValidator(new IdentifierValidator(this));

	setWindowTitle(QString("Signal %1").arg(m_signalEdge->signalName()));

	m_ui->nameLineEdit->setText(m_signalEdge->signalName());

	const Core::DatatypeName &messageDatatypeName = m_signalEdge->messageDatatypeName();

	if (messageDatatypeName.type() != Core::DatatypeName::Invalid)
	{
		m_ui->attachMessageGroupBox->setChecked(true);
		m_ui->editDatatypeNameWidget->setDatatypeName(messageDatatypeName);
	}
	else
	{
		m_ui->attachMessageGroupBox->setChecked(false);
		m_ui->editDatatypeNameWidget->setDatatypeName(Core::DatatypeName::makeBool());
	}
}

EditSignalEdgeDialog::~EditSignalEdgeDialog()
{
	delete m_ui;
}

void EditSignalEdgeDialog::setExistingDatatypeNamesList(const QStringList &list)
{
	m_ui->editDatatypeNameWidget->setExistingDatatypeNamesList(list);
}

void EditSignalEdgeDialog::accept()
{
	if (!StrictIdentifierValidator::checkSignalNameWithMessageBox(this, m_ui->nameLineEdit->text()))
		return;

	if (m_ui->attachMessageGroupBox->isChecked() == false) // no attached message
	{
		m_signalEdge->setMessageDatatypeName(Core::DatatypeName());
	}
	else // attached message
	{
		Core::DatatypeName dt = m_ui->editDatatypeNameWidget->datatypeName();
		Q_ASSERT(dt.type() != Core::DatatypeName::Invalid);

		const Core::DatatypeName *dtToCheck = (dt.type() == Core::DatatypeName::Set) ?
			dt.innerDatatype() : &dt;
		Q_ASSERT(dtToCheck->type() != Core::DatatypeName::Invalid);
		Q_ASSERT(dtToCheck->type() != Core::DatatypeName::Set);

		if (dtToCheck->type() == Core::DatatypeName::Other
			&& !StrictIdentifierValidator::checkDatatypeWithMessageBox(this, dtToCheck->datatypeName()))
		{
			return;
		}

		m_signalEdge->setMessageDatatypeName(dt);
	}

	m_signalEdge->setSignalName(m_ui->nameLineEdit->text());

	QDialog::accept();
}

}
