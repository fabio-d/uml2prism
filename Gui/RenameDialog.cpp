#include "Gui/RenameDialog.h"

#include "Gui/IdentifierValidator.h"

#include "Core/UMLElement.h"

#include <QPushButton>

#include "ui_RenameDialog.h"

namespace Gui
{

RenameDialog::RenameDialog(Core::UMLElement *elem, QWidget *parent)
: QDialog(parent), m_ui(new Ui_RenameDialog)
{
	m_nodeElem = dynamic_cast<Core::UMLNodeElement*>(elem);
	m_controlFlowElem = dynamic_cast<Core::UMLControlFlowEdge*>(elem);
	m_signalFlowElem = dynamic_cast<Core::UMLSignalEdge*>(elem);
	m_datatypeElem = dynamic_cast<Core::UMLDatatypeElement*>(elem);

	m_ui->setupUi(this);
	m_ui->lineEdit->setValidator(new IdentifierValidator(this));

	switch (elem->type())
	{
		case Core::UMLElementType::InitialNode:
			setWindowTitle(QString("Rename %1").arg(m_nodeElem->nodeName()));
			setWindowIcon(QIcon(":/topcased_icons/InitialNode_24.gif"));
			m_ui->label->setText("New node name");
			m_ui->lineEdit->setText(m_nodeElem->nodeName());
			break;
		case Core::UMLElementType::FinalNode:
			setWindowTitle(QString("Rename %1").arg(m_nodeElem->nodeName()));
			setWindowIcon(QIcon(":/topcased_icons/ActivityFinalNode_24.gif"));
			m_ui->label->setText("New node name");
			m_ui->lineEdit->setText(m_nodeElem->nodeName());
			break;
		case Core::UMLElementType::ActionNode:
			setWindowTitle(QString("Rename %1").arg(m_nodeElem->nodeName()));
			setWindowIcon(QIcon(":/topcased_icons/OpaqueAction_24.gif"));
			m_ui->label->setText("New node name");
			m_ui->lineEdit->setText(m_nodeElem->nodeName());
			break;
		case Core::UMLElementType::DecisionMergeNode:
			setWindowTitle(QString("Rename %1").arg(m_nodeElem->nodeName()));
			setWindowIcon(QIcon(":/topcased_icons/DecisionNode_24.gif"));
			m_ui->label->setText("New node name");
			m_ui->lineEdit->setText(m_nodeElem->nodeName());
			break;
		case Core::UMLElementType::ForkJoinNode:
			setWindowTitle(QString("Rename %1").arg(m_nodeElem->nodeName()));
			setWindowIcon(QIcon(":/topcased_icons/ForkNode_24.gif"));
			m_ui->label->setText("New node name");
			m_ui->lineEdit->setText(m_nodeElem->nodeName());
			break;
		case Core::UMLElementType::ControlFlowEdge:
			if (m_controlFlowElem->branchName().isEmpty())
				setWindowTitle("Rename branch");
			else
				setWindowTitle(QString("Rename [%1] branch").arg(m_controlFlowElem->branchName()));
			setWindowIcon(QIcon(":/topcased_icons/ControlFlow_24.gif"));
			m_ui->label->setText("New branch name");
			m_ui->lineEdit->setText(m_controlFlowElem->branchName());
			break;
		case Core::UMLElementType::SignalEdge:
			setWindowTitle(QString("Rename %1").arg(m_signalFlowElem->signalName()));
			setWindowIcon(QIcon(":/topcased_icons/ObjectFlow_24.gif"));
			m_ui->label->setText("New signal name");
			m_ui->lineEdit->setText(m_signalFlowElem->signalName());
			break;
		case Core::UMLElementType::Enumeration:
			setWindowTitle(QString("Rename %1").arg(m_datatypeElem->datatypeName()));
			setWindowIcon(QIcon(":/topcased_icons/Enumeration_24.gif"));
			m_ui->label->setText("New enumeration name");
			m_ui->lineEdit->setText(m_datatypeElem->datatypeName());
			break;
		case Core::UMLElementType::Class:
			setWindowTitle(QString("Rename %1").arg(m_datatypeElem->datatypeName()));
			setWindowIcon(QIcon(":/topcased_icons/Class_24.gif"));
			m_ui->label->setText("New class name");
			m_ui->lineEdit->setText(m_datatypeElem->datatypeName());
			break;
		case Core::UMLElementType::GlobalVariables:
			qFatal("This should never happen");
			break;
	}

	m_ui->lineEdit->selectAll();
}

RenameDialog::~RenameDialog()
{
	delete m_ui;
}

void RenameDialog::accept()
{
	if (m_nodeElem)
		m_nodeElem->setNodeName(m_ui->lineEdit->text());
	else if (m_controlFlowElem)
		m_controlFlowElem->setBranchName(m_ui->lineEdit->text());
	else if (m_signalFlowElem)
		m_signalFlowElem->setSignalName(m_ui->lineEdit->text());
	else if (m_datatypeElem)
		m_datatypeElem->setDatatypeName(m_ui->lineEdit->text());

	QDialog::accept();
}

void RenameDialog::slotTextChanged()
{
	if (m_controlFlowElem != nullptr) // branches can be empty
		return;

	const bool validInput = StrictIdentifierValidator::isAcceptable(m_ui->lineEdit->text());
	m_ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(validInput);
}

}
