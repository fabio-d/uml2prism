#include "Gui/OutputDialog.h"

#include "ui_OutputDialog.h"

namespace Gui
{

OutputDialog::OutputDialog(const QString &model, const QString &properties, QWidget *parent)
: QDialog(parent), m_ui(new Ui_OutputDialog)
{
	m_ui->setupUi(this);

	m_ui->modelText->setPlainText(model);
	m_ui->propertiesText->setPlainText(properties);
}

OutputDialog::~OutputDialog()
{
	delete m_ui;
}

}
