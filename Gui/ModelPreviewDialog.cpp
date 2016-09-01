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
