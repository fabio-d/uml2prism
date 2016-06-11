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
	Core::ModelBuilder builder(m_doc);
	if (builder.run())
	{
		Core::Compiler::SemanticTreeGenerator stgen(
			m_ui->expressionTextEdit->toPlainText(),
			builder.semanticContext(),
			builder.semanticContext()->boolType(),
			m_predType == Core::PredicateType::Property);
		if (stgen.success())
		{
			const Core::Compiler::SemanticTree::Expr *semTree = stgen.takeResultExpr();
			qDebug() << "C++ semantic tree:";
			qDebug() << semTree->toString();
			qDebug() << "Haskell semantic tree:";
			hsExpr_dump(semTree->haskellHandle());
			delete semTree;
		}
	}
}

}
