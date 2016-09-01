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

#include "Gui/EditPredicateDialog.h"

#include "Gui/IdentifierValidator.h"

#include "Core/Compiler/SemanticTreeGenerator.h"
#include "Core/ModelBuilder.h"
#include "Core/PredicateList.h"

#include <QPushButton>
#include <QDebug>

#include "ui_EditPredicateDialog.h"

namespace Gui
{

EditPredicateDialog::EditPredicateDialog(Core::Document *doc, Core::Predicate *pred, Core::PredicateType type, QWidget *parent)
: QDialog(parent), m_ui(new Ui_EditPredicateDialog), m_doc(doc), m_pred(pred),
  m_predType(type)
{
	m_ui->setupUi(this);
	m_ui->nameLineEdit->setValidator(new IdentifierValidator(this));

	switch (type)
	{
		case Core::PredicateType::Property:
			m_ui->label->setText("Property name");
			break;
		case Core::PredicateType::Label:
			m_ui->label->setText("Label name");
			break;
	}

	setWindowTitle(QString("Edit %1").arg(m_pred->name()));
	m_ui->nameLineEdit->setText(m_pred->name());
	m_ui->expressionTextEdit->setPlainText(m_pred->expression());
}

EditPredicateDialog::~EditPredicateDialog()
{
	delete m_ui;
}

void EditPredicateDialog::accept()
{
	if (!StrictIdentifierValidator::checkNodeNameWithMessageBox(this, m_ui->nameLineEdit->text()))
		return;

	m_pred->setName(m_ui->nameLineEdit->text());
	m_pred->setExpression(m_ui->expressionTextEdit->toPlainText());
	QDialog::accept();
}

}
