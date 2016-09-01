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

#ifndef GUI_MODELRUNDIALOG_H
#define GUI_MODELRUNDIALOG_H

#include <QDialog>
#include <QProcess>
#include <QTemporaryFile>

class Ui_ModelRunDialog;

namespace Core
{
class PredicateList;
}

namespace Gui
{

class ModelRunDialog : public QDialog
{
	Q_OBJECT

	public:
		ModelRunDialog(const Core::PredicateList *propertyList, const QString &model, const QString &propertiesPCTL, const QString &propertiesCTL, QWidget *parent = nullptr);
		~ModelRunDialog();

	private slots:
		void slotBrowse();
		void slotProcessReadyRead();
		void slotProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);
		void slotSetDefaultPrismExecutable(const QString &newValue);

	private:
		void accept() override;
		void start();
		void stop();
		void logAppend(const QString &text);
		void prismFinished(bool success);

		Ui_ModelRunDialog *m_ui;
		QPushButton *m_startStopButton;

		const Core::PredicateList *m_propertyList;

		bool m_started;
		QString m_nm, m_pctl, m_ctl;

		QProcess m_prismProcess;
		QString m_prismProcessOutput;
		QScopedPointer<QTemporaryFile> m_modelTempFile, m_propertiesTempFile, m_resultsTempFile;
};

}

#endif // GUI_MODELRUNDIALOG_H
