#include "Gui/EditEnumerationDialog.h"

#include "Core/UMLElement.h"

#include "ui_EditEnumerationDialog.h"

namespace Gui
{

EditEnumerationDialog::EditEnumerationDialog(Core::UMLEnumeration *enumeration, QWidget *parent)
: QDialog(parent), m_ui(new Ui_EditEnumerationDialog),
  m_enumeration(enumeration)
{
	m_ui->setupUi(this);

	setWindowTitle(QString("Enumeration %1").arg(m_enumeration->datatypeName()));

	m_ui->nameLineEdit->setText(m_enumeration->datatypeName());
	m_ui->valuesListWidget->addItems(m_enumeration->values());

	slotSelectedRowChanged();
}

EditEnumerationDialog::~EditEnumerationDialog()
{
	delete m_ui;
}

void EditEnumerationDialog::accept()
{
	QStringList values;

	for (int i = 0; i < m_ui->valuesListWidget->count(); i++)
		values.append(m_ui->valuesListWidget->item(i)->text());

	m_enumeration->setDatatypeName(m_ui->nameLineEdit->text());
	m_enumeration->setValues(values);

	QDialog::accept();
}

void EditEnumerationDialog::slotAdd()
{
	m_ui->valuesListWidget->addItem("NewValue");
	m_ui->valuesListWidget->setCurrentRow(m_ui->valuesListWidget->count() - 1);
}

void EditEnumerationDialog::slotRemove()
{
	const int currentRow = m_ui->valuesListWidget->currentRow();
	Q_ASSERT(currentRow >= 0);
	delete m_ui->valuesListWidget->takeItem(currentRow);
}

void EditEnumerationDialog::slotMoveUp()
{
	const int currentRow = m_ui->valuesListWidget->currentRow();
	Q_ASSERT(currentRow > 0);
	m_ui->valuesListWidget->insertItem(currentRow - 1, m_ui->valuesListWidget->takeItem(currentRow));
	m_ui->valuesListWidget->setCurrentRow(currentRow - 1);
}

void EditEnumerationDialog::slotMoveDown()
{
	const int currentRow = m_ui->valuesListWidget->currentRow();
	Q_ASSERT(currentRow >= 0 && currentRow != m_ui->valuesListWidget->count() - 1);
	m_ui->valuesListWidget->insertItem(currentRow, m_ui->valuesListWidget->takeItem(currentRow + 1));
	m_ui->valuesListWidget->setCurrentRow(currentRow + 1);

	slotSelectedRowChanged();
	m_ui->valuesListWidget->scrollToItem(m_ui->valuesListWidget->currentItem());
}

void EditEnumerationDialog::slotSelectedRowChanged()
{
	const int newRow = m_ui->valuesListWidget->currentRow();

	if (newRow == -1)
	{
		m_ui->valueLineEdit->setEnabled(false);
		m_ui->valueLineEdit->setText(QString());
		m_ui->removePushButton->setEnabled(false);
		m_ui->moveUpPushButton->setEnabled(false);
		m_ui->moveDownPushButton->setEnabled(false);
	}
	else
	{
		m_ui->valueLineEdit->setEnabled(true);
		m_ui->valueLineEdit->setText(m_ui->valuesListWidget->currentItem()->text());
		m_ui->removePushButton->setEnabled(true);
		m_ui->moveUpPushButton->setEnabled(newRow != 0);
		m_ui->moveDownPushButton->setEnabled(newRow != m_ui->valuesListWidget->count() - 1);
	}
}

void EditEnumerationDialog::slotEdited()
{
	Q_ASSERT(m_ui->valuesListWidget->currentRow() != -1);
	m_ui->valuesListWidget->currentItem()->setText(m_ui->valueLineEdit->text());
}

}
