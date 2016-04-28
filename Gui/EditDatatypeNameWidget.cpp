#include "Gui/EditDatatypeNameWidget.h"

#include "Gui/IdentifierValidator.h"

#include "ui_EditDatatypeNameWidget.h"

namespace Gui
{

EditDatatypeNameWidget::EditDatatypeNameWidget(QWidget *parent)
: QWidget(parent), m_ui(new Ui_EditDatatypeNameWidget), m_ignoreEditSignals(false)
{
	m_ui->setupUi(this);

	m_ui->rangeFromLineEdit->setValidator(new QIntValidator(this));
	m_ui->rangeToLineEdit->setValidator(new QIntValidator(this));
	m_ui->otherComboBox->setValidator(new IdentifierValidator(this));
}

EditDatatypeNameWidget::~EditDatatypeNameWidget()
{
	delete m_ui;
}

void EditDatatypeNameWidget::setDatatypeName(const Core::DatatypeName &dt)
{
	m_ignoreEditSignals = true;

	const bool isSet = dt.type() == Core::DatatypeName::Set;
	const Core::DatatypeName *datatype = isSet ? dt.innerDatatype() : &dt;

	m_ui->setCheckBox->setChecked(isSet);
	m_ui->rangeFromLineEdit->setText(QString());
	m_ui->rangeFromLineEdit->setEnabled(false);
	m_ui->rangeToLineEdit->setText(QString());
	m_ui->rangeToLineEdit->setEnabled(false);
	m_ui->otherComboBox->setEditText(QString());
	m_ui->otherComboBox->setEnabled(false);

	switch (datatype->type())
	{
		case Core::DatatypeName::Bool:
			m_ui->boolRadioButton->setChecked(true);
			break;
		case Core::DatatypeName::Integer:
			m_ui->integerRadioButton->setChecked(true);
			m_ui->rangeFromLineEdit->setText(QString::number(datatype->integerRangeFrom()));
			m_ui->rangeFromLineEdit->setEnabled(true);
			m_ui->rangeToLineEdit->setText(QString::number(datatype->integerRangeTo()));
			m_ui->rangeToLineEdit->setEnabled(true);
			break;
		case Core::DatatypeName::Other:
			m_ui->otherRadioButton->setChecked(true);
			m_ui->otherComboBox->setEnabled(true);
			m_ui->otherComboBox->setEditText(datatype->datatypeName());
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
		m_ui->integerRadioButton->isChecked() ? Core::DatatypeName::makeInteger(
			m_ui->rangeFromLineEdit->text().toInt(), m_ui->rangeToLineEdit->text().toInt()) :
		Core::DatatypeName::makeOther(m_ui->otherComboBox->currentText());
	return m_ui->setCheckBox->isChecked() ? Core::DatatypeName::makeSet(res) : res;
}

void EditDatatypeNameWidget::slotSomethingChanged()
{
	if (m_ignoreEditSignals == false)
		emit datatypeEdited();
}

void EditDatatypeNameWidget::slotIntegerRadioButtonToggled(bool checked)
{
	m_ui->rangeFromLineEdit->setEnabled(checked);
	m_ui->rangeToLineEdit->setEnabled(checked);
}

void EditDatatypeNameWidget::slotOtherRadioButtonToggled(bool checked)
{
	m_ui->otherComboBox->setEnabled(checked);
}

}
