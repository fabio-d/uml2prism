#include "Gui/ModelRunDialog.h"

#include "ui_ModelRunDialog.h"

#include <QDebug>
#include <QFileDialog>
#include <QMessageBox>

namespace Gui
{

ModelRunDialog::ModelRunDialog(const QString &model, const QString &propertiesPCTL, const QString &propertiesCTL, QWidget *parent)
: QDialog(parent), m_ui(new Ui_ModelRunDialog), m_started(false),
  m_nm(model), m_pctl(propertiesPCTL), m_ctl(propertiesCTL),
  m_modelTempFile(QDir::tempPath() + "/XXXXXX.prism"),
  m_propertiesTempFile(QDir::tempPath() + "/XXXXXX.props")
{
	m_ui->setupUi(this);
	m_ui->progressBar->hide();

	m_startStopButton = m_ui->buttonBox->addButton("Run PRISM", QDialogButtonBox::AcceptRole);

	m_prismProcess.setProcessChannelMode(QProcess::MergedChannels);
	connect(&m_prismProcess, SIGNAL(readyRead()), this, SLOT(slotProcessReadyRead()));
	connect(&m_prismProcess, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(slotProcessFinished(int,QProcess::ExitStatus)));
}

ModelRunDialog::~ModelRunDialog()
{
	if (m_started)
		stop();

	delete m_ui;
}

void ModelRunDialog::slotBrowse()
{
	const QString selectedFileName = QFileDialog::getOpenFileName(this,
		"Locate PRISM executable", m_ui->prismExecutableLineEdit->text(), "PRISM executable (prism)");

	if (selectedFileName.isEmpty())
		return;

	m_ui->prismExecutableLineEdit->setText(selectedFileName);
}

void ModelRunDialog::accept() // m_startStopButton pressed
{
	if (m_started)
		stop();
	else
		start();
}

void ModelRunDialog::start()
{
	if (m_started)
		return;

	QStringList prismArgs;

	m_modelTempFile.open();
	m_modelTempFile.write(m_nm.toLatin1());
	m_modelTempFile.close();

	m_propertiesTempFile.open();
	if (m_ui->logicPctlRadioButton->isChecked())
		m_propertiesTempFile.write(m_pctl.toLatin1());
	else if (m_ui->logicCtlRadioButton->isChecked())
		m_propertiesTempFile.write(m_ctl.toLatin1());
	else
		qFatal("This should never happen");
	m_propertiesTempFile.close();

	prismArgs << m_modelTempFile.fileName();
	prismArgs << m_propertiesTempFile.fileName();

	if (m_ui->engExplicitRadioButton->isChecked())
		prismArgs << "-ex";
	else if (m_ui->engHybridRadioButton->isChecked())
		prismArgs << "-h";
	else if (m_ui->engSparseRadioButton->isChecked())
		prismArgs << "-s";
	else if (m_ui->engMtbddRadioButton->isChecked())
		prismArgs << "-m";
	else
		qFatal("This should never happen");

	prismArgs += m_ui->extraOptionsLineEdit->text().split(' ', QString::SkipEmptyParts);
	m_ui->rawLogText->clear();
	m_ui->rawLogText->setPlainText(QString("STARTING %1 %2\n").arg(m_ui->prismExecutableLineEdit->text()).arg(prismArgs.join(" ")));

	m_prismProcess.start(m_ui->prismExecutableLineEdit->text(), prismArgs);
	m_prismProcess.waitForStarted();

	if (m_prismProcess.state() != QProcess::Running)
	{
		logAppend("FAILED to start\n");
		QMessageBox::critical(this, "Run PRISM", "Failed to start PRISM process, please check PRISM executable path");
	}
	else
	{
		m_started = true;
		m_startStopButton->setText("Kill PRISM");
		m_ui->progressBar->show();
		m_ui->setupTab->setEnabled(false);
	}
}

void ModelRunDialog::stop()
{
	if (!m_started)
		return;

	m_started = false;
	m_startStopButton->setText("Run PRISM");
	m_ui->progressBar->hide();
	m_ui->setupTab->setEnabled(true);
	m_prismProcess.terminate();
	m_prismProcess.waitForFinished();

	logAppend("KILLED by the user\n");
}

void ModelRunDialog::slotProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
	m_started = false;
	m_startStopButton->setText("Run PRISM");
	m_ui->progressBar->hide();
	m_ui->setupTab->setEnabled(true);

	logAppend(QString("FINISHED with exit code %1, status is %2").arg(exitCode).arg(exitStatus == QProcess::NormalExit ? "NormalExit" : "CrashExit"));
}

void ModelRunDialog::slotProcessReadyRead()
{
	QString newData;
	QByteArray readRes;

	while ((readRes = m_prismProcess.read(512)).isEmpty() == false)
		newData += readRes;

	logAppend(newData);
}

void ModelRunDialog::logAppend(const QString &text)
{
	const QTextCursor prev_cursor = m_ui->rawLogText->textCursor();
	m_ui->rawLogText->moveCursor(QTextCursor::End);
	m_ui->rawLogText->insertPlainText(text);
	m_ui->rawLogText->setTextCursor(prev_cursor);
}

}
