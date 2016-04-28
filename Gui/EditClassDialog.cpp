#include "Gui/EditClassDialog.h"

#include "Gui/IdentifierValidator.h"

#include "Core/UMLElement.h"

#include <QDebug>
#include <QDomDocument>

#include "ui_EditClassDialog.h"

static QVariant packQVariant(const Core::UMLClass::MemberVariable &variable)
{
	QDomDocument doc;
	QDomElement rootElem = doc.createElement("variable");
	QDomElement nameElem = doc.createElement("name");
	QDomElement datatypeElem = doc.createElement("datatype");
	doc.appendChild(rootElem);
	rootElem.appendChild(nameElem);
	rootElem.appendChild(datatypeElem);
	nameElem.appendChild(doc.createTextNode(variable.name));
	variable.datatypeName.storeToXml(datatypeElem, doc);
	return doc.toByteArray(4);
}

static QVariant packQVariant(const QString &name, const Core::DatatypeName &datatypeName)
{
	return packQVariant(Core::UMLClass::MemberVariable(name, datatypeName));
}

static Core::UMLClass::MemberVariable unpackQVariant(const QVariant &data)
{
	QDomDocument doc;
	if (!doc.setContent(data.toByteArray()))
		return Core::UMLClass::MemberVariable();

	QDomElement rootElem = doc.documentElement();
	QDomElement nameElem = rootElem.firstChildElement("name");
	QDomElement datatypeElem = rootElem.firstChildElement("datatype");

	return Core::UMLClass::MemberVariable(nameElem.text(), Core::DatatypeName(datatypeElem));
}

namespace Gui
{

EditClassDialog::EditClassDialog(Core::UMLClass *class_, QWidget *parent)
: QDialog(parent), m_ui(new Ui_EditClassDialog),
  m_class(class_)
{
	m_ui->setupUi(this);
	m_ui->editListWidget->setCallbacks(this);

	m_ui->nameLineEdit->setValidator(new IdentifierValidator(this));
	m_ui->varNameLineEdit->setValidator(new IdentifierValidator(this));

	setWindowTitle(QString("Class %1").arg(m_class->datatypeName()));

	m_ui->nameLineEdit->setText(m_class->datatypeName());

	QList<QVariant> varValues;
	foreach (const Core::UMLClass::MemberVariable &var, m_class->memberVariables())
		varValues.append(packQVariant(var));
	m_ui->editListWidget->setValues(varValues);
}

EditClassDialog::~EditClassDialog()
{
	delete m_ui;
}

void EditClassDialog::accept()
{
	if (!StrictIdentifierValidator::checkDatatypeWithMessageBox(this, m_ui->nameLineEdit->text()))
		return;

	QList<Core::UMLClass::MemberVariable> values;
	foreach (const QVariant &val, m_ui->editListWidget->values())
		values.append(unpackQVariant(val));

	m_class->setDatatypeName(m_ui->nameLineEdit->text());
	m_class->setMemberVariables(values);

	QDialog::accept();
}

void EditClassDialog::slotEdited()
{
	m_ui->editListWidget->setEditedData(packQVariant(
		m_ui->varNameLineEdit->text(),
		m_ui->editDatatypeNameWidget->datatypeName()));
}

void EditClassDialog::setEditorData(const QVariant &initialData)
{
	const Core::UMLClass::MemberVariable var = unpackQVariant(initialData);
	m_ui->varNameLineEdit->setText(var.name);
	m_ui->editDatatypeNameWidget->setDatatypeName(var.datatypeName);
}

void EditClassDialog::setEditorEnabled(bool enable)
{
	m_ui->varNameLineEdit->setEnabled(enable);
	m_ui->editDatatypeNameWidget->setEnabled(enable);
}

QString EditClassDialog::formatData(const QVariant &data)
{
	const Core::UMLClass::MemberVariable var = unpackQVariant(data);
	return QString("%1 : %2").arg(var.name)
		.arg(var.datatypeName.toString());
}

QVariant EditClassDialog::generateEmptyEntry()
{
	return packQVariant("", Core::DatatypeName::makeBool());
}

bool EditClassDialog::testEntryEmpty(const QVariant &data)
{
	const Core::UMLClass::MemberVariable var = unpackQVariant(data);
	const Core::DatatypeName *nonSetDatatype =
		var.datatypeName.type() == Core::DatatypeName::Set ?
		var.datatypeName.innerDatatype() : &var.datatypeName;

	Q_ASSERT(nonSetDatatype->type() != Core::DatatypeName::Set);

	return var.name.isEmpty() || (nonSetDatatype->type() == Core::DatatypeName::Other
		&& nonSetDatatype->datatypeName().isEmpty());
}

}
