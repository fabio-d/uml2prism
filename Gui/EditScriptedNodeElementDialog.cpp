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

	QPushButton *parseButton = m_ui->buttonBox->addButton("Parse", QDialogButtonBox::ActionRole);
	connect(parseButton, SIGNAL(clicked()), this, SLOT(slotParse()));
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

void EditScriptedNodeElementDialog::slotParse()
{
	Core::ModelBuilder builder(m_doc);
	if (builder.success())
	{
		QMap<QString, QString> tmpMap;
		tmpMap.insert("yes", "DestNodeYY");
		tmpMap.insert("no", "DestNodeNN");
		Core::Compiler::SemanticTreeGenerator stgen(
			m_ui->scriptTextEdit->toPlainText(),
			builder.semanticContext(),
			QStringList() << "HelloServer",
			tmpMap);
		if (stgen.success())
			qDebug() << stgen.takeResultStmt()->toString();
	}
}

}
