#ifndef GUI_MODELRUNDIALOG_H
#define GUI_MODELRUNDIALOG_H

#include <QDialog>
#include <QProcess>
#include <QTemporaryFile>

class Ui_ModelRunDialog;

namespace Gui
{

class ModelRunDialog : public QDialog
{
	Q_OBJECT

	public:
		ModelRunDialog(const QString &model, const QString &propertiesPCTL, const QString &propertiesCTL, QWidget *parent = nullptr);
		~ModelRunDialog();

	private slots:
		void slotBrowse();
		void slotProcessReadyRead();
		void slotProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);

	private:
		void accept() override;
		void start();
		void stop();
		void logAppend(const QString &text);

		Ui_ModelRunDialog *m_ui;
		QPushButton *m_startStopButton;

		bool m_started;
		QString m_nm, m_pctl, m_ctl;

		QProcess m_prismProcess;
		QTemporaryFile m_modelTempFile, m_propertiesTempFile;
};

}

#endif // GUI_MODELRUNDIALOG_H
