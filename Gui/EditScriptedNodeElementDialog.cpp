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

#include "Gui/EditScriptedNodeElementDialog.h"

#include "Gui/IdentifierValidator.h"

#include "Core/Compiler/SemanticTreeGenerator.h"
#include "Core/ModelBuilder.h"
#include "Core/UMLElement.h"

#include <QPushButton>
#include <QDebug>

#include "ui_EditScriptedNodeElementDialog.h"

namespace Gui
{

EditScriptedNodeElementDialog::EditScriptedNodeElementDialog(Core::Document *doc, Core::UMLScriptedNodeElement *elem, QWidget *parent)
: QDialog(parent), m_ui(new Ui_EditScriptedNodeElementDialog), m_doc(doc), m_elem(elem)
{
	m_ui->setupUi(this);
	m_ui->nameLineEdit->setValidator(new IdentifierValidator(this));

	switch (elem->type())
	{
		case Core::UMLElementType::ActionNode:
			setWindowIcon(QIcon(":/topcased_icons/OpaqueAction_24.gif"));
			break;
		case Core::UMLElementType::DecisionMergeNode:
			setWindowIcon(QIcon(":/topcased_icons/DecisionNode_24.gif"));
			break;
		default:
			qFatal("This should never happen");
			break;
	}

	setWindowTitle(QString("Edit %1").arg(m_elem->nodeName()));
	m_ui->nameLineEdit->setText(m_elem->nodeName());

	if (m_elem->hasCustomScript())
	{
		m_ui->customScriptGroupBox->setChecked(true);
		m_ui->scriptTextEdit->setPlainText(m_elem->customScript());
	}
	else
	{
		m_ui->customScriptGroupBox->setChecked(false);
	}
}

EditScriptedNodeElementDialog::~EditScriptedNodeElementDialog()
{
	delete m_ui;
}

void EditScriptedNodeElementDialog::accept()
{
	if (!StrictIdentifierValidator::checkNodeNameWithMessageBox(this, m_ui->nameLineEdit->text()))
		return;

	m_elem->setNodeName(m_ui->nameLineEdit->text());

	if (m_ui->customScriptGroupBox->isChecked())
		m_elem->setCustomScript(m_ui->scriptTextEdit->toPlainText());
	else
		m_elem->unsetCustomScript();
	QDialog::accept();
}

}
