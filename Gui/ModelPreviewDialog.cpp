#include "Gui/ModelPreviewDialog.h"

#include "ui_ModelPreviewDialog.h"

namespace Gui
{

ModelPreviewDialog::ModelPreviewDialog(const QString &model, const QString &propertiesPCTL, const QString &propertiesCTL, QWidget *parent)
: QDialog(parent), m_ui(new Ui_ModelPreviewDialog),
  m_nm(model), m_pctl(propertiesPCTL), m_ctl(propertiesCTL)
{
	m_ui->setupUi(this);

	m_ui->modelText->setPlainText(model);
	m_ui->propertiesText->setPlainText(propertiesPCTL);
}

ModelPreviewDialog::~ModelPreviewDialog()
{
	delete m_ui;
}

void ModelPreviewDialog::propTypeChanged()
{
	if (m_ui->pctlRadioButton->isChecked())
		m_ui->propertiesText->setPlainText(m_pctl);
	else
		m_ui->propertiesText->setPlainText(m_ctl);
}

}
