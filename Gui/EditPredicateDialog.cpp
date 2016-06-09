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

EditPredicateDialog::EditPredicateDialog(Core::Predicate *pred, Core::PredicateType type, QWidget *parent)
: QDialog(parent), m_ui(new Ui_EditPredicateDialog), m_pred(pred), m_predType(type)
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

	QPushButton *parseButton = m_ui->buttonBox->addButton("Parse", QDialogButtonBox::ActionRole);
	connect(parseButton, SIGNAL(clicked()), this, SLOT(slotParse()));
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

void EditPredicateDialog::slotParse()
{
	qDebug() << "Not implemented";
}

}
