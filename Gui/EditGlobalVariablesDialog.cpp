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

#include "Gui/EditGlobalVariablesDialog.h"

#include "Gui/IdentifierValidator.h"

#include "Core/UMLElement.h"

#include <QDebug>
#include <QDomDocument>

#include "ui_EditGlobalVariablesDialog.h"

static QVariant packQVariant(const Core::UMLGlobalVariables::GlobalVariable &variable)
{
	QDomDocument doc;
	QDomElement rootElem = doc.createElement("variable");
	QDomElement nameElem = doc.createElement("name");
	QDomElement datatypeElem = doc.createElement("datatype");
	QDomElement initValElem = doc.createElement("initial-value");
	doc.appendChild(rootElem);
	rootElem.appendChild(nameElem);
	rootElem.appendChild(datatypeElem);
	rootElem.appendChild(initValElem);
	rootElem.setAttribute("persistent", variable.isPersistent ? "true" : "false");
	nameElem.appendChild(doc.createTextNode(variable.name));
	variable.datatypeName.storeToXml(datatypeElem, doc);
	initValElem.appendChild(doc.createTextNode(variable.initialValue));
	return doc.toByteArray(4);
}

static QVariant packQVariant(const QString &name, const Core::DatatypeName &datatypeName, bool isPersistent, const QString &initialValue)
{
	return packQVariant(Core::UMLGlobalVariables::GlobalVariable(name, datatypeName, isPersistent, initialValue));
}

static Core::UMLGlobalVariables::GlobalVariable unpackQVariant(const QVariant &data)
{
	QDomDocument doc;
	if (!doc.setContent(data.toByteArray()))
		return Core::UMLGlobalVariables::GlobalVariable();

	QDomElement rootElem = doc.documentElement();
	QDomElement nameElem = rootElem.firstChildElement("name");
	QDomElement datatypeElem = rootElem.firstChildElement("datatype");
	QDomElement initValElem = rootElem.firstChildElement("initial-value");

	return Core::UMLGlobalVariables::GlobalVariable(nameElem.text(),
		Core::DatatypeName(datatypeElem),
		rootElem.attribute("persistent") == "true", initValElem.text());
}

namespace Gui
{

EditGlobalVariablesDialog::EditGlobalVariablesDialog(Core::UMLGlobalVariables *globalVariables, QWidget *parent)
: QDialog(parent), m_ui(new Ui_EditGlobalVariablesDialog),
  m_globalVariables(globalVariables), m_ignoreEditSignals(false)
{
	m_ui->setupUi(this);
	m_ui->editListWidget->setCallbacks(this);

	m_ui->varNameLineEdit->setValidator(new IdentifierValidator(this));

	QList<QVariant> varValues;
	foreach (const Core::UMLGlobalVariables::GlobalVariable &var, m_globalVariables->globalVariables())
		varValues.append(packQVariant(var));
	m_ui->editListWidget->setValues(varValues);
}

EditGlobalVariablesDialog::~EditGlobalVariablesDialog()
{
	delete m_ui;
}

void EditGlobalVariablesDialog::setExistingDatatypeNamesList(const QStringList &list)
{
	m_ui->editDatatypeNameWidget->setExistingDatatypeNamesList(list);
}

void EditGlobalVariablesDialog::accept()
{
	QList<Core::UMLGlobalVariables::GlobalVariable> values;
	foreach (const QVariant &val, m_ui->editListWidget->values())
	{
		const Core::UMLGlobalVariables::GlobalVariable var = unpackQVariant(val);
		if (!StrictIdentifierValidator::checkVariableNameWithMessageBox(this, var.name))
			return;
		values.append(var);
	}

	m_globalVariables->setGlobalVariables(values);

	QDialog::accept();
}

void EditGlobalVariablesDialog::slotEdited()
{
	if (m_ignoreEditSignals)
		return;

	m_ui->editListWidget->setEditedData(packQVariant(
		m_ui->varNameLineEdit->text(),
		m_ui->editDatatypeNameWidget->datatypeName(),
		m_ui->persistentCheckBox->isChecked(),
		m_ui->initialValueTextEdit->toPlainText()));
}

void EditGlobalVariablesDialog::setEditorData(const QVariant &initialData)
{
	m_ignoreEditSignals = true;

	const Core::UMLGlobalVariables::GlobalVariable var = unpackQVariant(initialData);
	m_ui->varNameLineEdit->setText(var.name);
	m_ui->editDatatypeNameWidget->setDatatypeName(var.datatypeName);
	m_ui->persistentCheckBox->setChecked(var.isPersistent);
	m_ui->initialValueTextEdit->setPlainText(var.initialValue);

	m_ignoreEditSignals = false;
}

void EditGlobalVariablesDialog::setEditorEnabled(bool enable)
{
	m_ui->varNameLineEdit->setEnabled(enable);
	m_ui->editDatatypeNameWidget->setEnabled(enable);
	m_ui->persistentCheckBox->setEnabled(enable);
	m_ui->initialValueTextEdit->setEnabled(enable);
}

QString EditGlobalVariablesDialog::formatData(const QVariant &data)
{
	const Core::UMLGlobalVariables::GlobalVariable var = unpackQVariant(data);
	return QString("%1 : %2 = %3")
		.arg(var.name)
		.arg(var.datatypeName.toString())
		.arg(var.initialValue);
}

void EditGlobalVariablesDialog::formatFont(const QVariant &data, QFont &font)
{
	font.setUnderline(unpackQVariant(data).isPersistent);
}

QVariant EditGlobalVariablesDialog::generateEmptyEntry()
{
	return packQVariant("", Core::DatatypeName::makeBool(), false, "nil");
}

bool EditGlobalVariablesDialog::testEntryEmpty(const QVariant &data)
{
	const Core::UMLGlobalVariables::GlobalVariable var = unpackQVariant(data);
	const Core::DatatypeName *nonSetDatatype =
		var.datatypeName.type() == Core::DatatypeName::Set ?
		var.datatypeName.innerDatatype() : &var.datatypeName;

	Q_ASSERT(nonSetDatatype->type() != Core::DatatypeName::Set);

	return var.name.isEmpty() || (nonSetDatatype->type() == Core::DatatypeName::Other
		&& nonSetDatatype->datatypeName().isEmpty());
}

}
