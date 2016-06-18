#include "Gui/ModelRunDialog.h"

#include "Core/PredicateList.h"

#include "ui_ModelRunDialog.h"

#include <QDebug>
#include <QFileDialog>
#include <QMessageBox>
#include <QScrollBar>

static QString defaultPrismExecutable = "prism";

namespace Gui
{

ModelRunDialog::ModelRunDialog(const Core::PredicateList *propertyList, const QString &model, const QString &propertiesPCTL, const QString &propertiesCTL, QWidget *parent)
: QDialog(parent), m_ui(new Ui_ModelRunDialog), m_propertyList(propertyList),
  m_started(false), m_nm(model), m_pctl(propertiesPCTL), m_ctl(propertiesCTL)
{
	m_ui->setupUi(this);
	m_ui->progressBar->hide();

	m_ui->prismExecutableLineEdit->setText(defaultPrismExecutable);
	connect(m_ui->prismExecutableLineEdit, SIGNAL(textChanged(QString)), this, SLOT(slotSetDefaultPrismExecutable(QString)));

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

	m_modelTempFile.reset(new QTemporaryFile(QDir::tempPath() + "/XXXXXX.prism"));
	m_propertiesTempFile.reset(new QTemporaryFile(QDir::tempPath() + "/XXXXXX.props"));
	m_resultsTempFile.reset(new QTemporaryFile(QDir::tempPath() + "/XXXXXX.out"));

	m_modelTempFile->open();
	m_modelTempFile->write(m_nm.toLatin1());
	m_modelTempFile->close();

	m_propertiesTempFile->open();
	if (m_ui->logicPctlRadioButton->isChecked())
		m_propertiesTempFile->write(m_pctl.toLatin1());
	else if (m_ui->logicCtlRadioButton->isChecked())
		m_propertiesTempFile->write(m_ctl.toLatin1());
	else
		qFatal("This should never happen");
	m_propertiesTempFile->close();

	// This is an output file, but we have to open it beforehand or Qt will
	// not created a temporary path
	m_resultsTempFile->open();
	m_resultsTempFile->close();

	prismArgs << m_modelTempFile->fileName();
	prismArgs << m_propertiesTempFile->fileName();
	prismArgs << "-exportresults" << m_resultsTempFile->fileName() + ":matrix";

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

	m_prismProcessOutput.clear();
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
		m_ui->tabWidget->setCurrentIndex(2);
		m_ui->reportText->setHtml("Please wait, PRISM is still running...");
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

	logAppend(QString("FINISHED with exit code %1, status is %2\n").arg(exitCode).arg(exitStatus == QProcess::NormalExit ? "NormalExit" : "CrashExit"));

	prismFinished(exitCode == 0 && exitStatus == QProcess::NormalExit);
	m_ui->tabWidget->setCurrentIndex(1);
}

void ModelRunDialog::slotProcessReadyRead()
{
	QString newData;
	QByteArray readRes;

	while ((readRes = m_prismProcess.read(512)).isEmpty() == false)
		newData += readRes;

	logAppend(newData);
	m_prismProcessOutput += newData;
}

void ModelRunDialog::logAppend(const QString &text)
{
	QScrollBar *hBar = m_ui->rawLogText->horizontalScrollBar();
	QScrollBar *vBar = m_ui->rawLogText->verticalScrollBar();
	int hVal = hBar->value();
	int vVal = vBar->value() == vBar->maximum() ? -1 : vBar->value();

	const QTextCursor prev_cursor = m_ui->rawLogText->textCursor();
	m_ui->rawLogText->moveCursor(QTextCursor::End);
	m_ui->rawLogText->insertPlainText(text);
	m_ui->rawLogText->setTextCursor(prev_cursor);

	hBar->setValue(hVal);
	vBar->setValue(vVal == -1 ? vBar->maximum() : vVal);
}

void ModelRunDialog::slotSetDefaultPrismExecutable(const QString &newValue)
{
	defaultPrismExecutable = newValue;
}

void ModelRunDialog::prismFinished(bool success)
{
	if (!success)
	{
		m_ui->reportText->setHtml("Oops! Something did not work properly.<br/>Please check out the <i>Raw PRISM output</i> tab.");
		return;
	}

	QString reportHtml = "<h1>Model information</h1>";

	QRegExp re("\nStates: +(\\d+) \\((\\d+) initial\\)\nTransitions: +(\\d+)\n");
	if (re.indexIn(m_prismProcessOutput) != -1)
	{
		reportHtml += QString("<p><b>States</b>: %1<br/><b>Initial states</b>: %2<br/><b>Transitions</b>: %3</p>")
			.arg(re.cap(1)).arg(re.cap(2)).arg(re.cap(3));
	}
	else
	{
		reportHtml += "<p>Failed to retrieve data</p>";
	}

	reportHtml += "<h1>Property truth values</h1>";

	QStringList propertiesResults;

	if (m_resultsTempFile->open())
	{
		// This file contains each property on its own line followed by
		// its truth value (or an error under some circumstances).
		// We are only interested in truth values
		bool evenLine = false;
		QTextStream in(m_resultsTempFile.data());
		while (!in.atEnd())
		{
			QString line = in.readLine().trimmed();
			if (line.isEmpty())
				continue;

			if (evenLine == true)
				propertiesResults << line;

			evenLine = !evenLine;
		}
		m_resultsTempFile->close();
	}

	const QList<Core::Predicate> &propertyList = m_propertyList->predicates();
	if (propertyList.count() == propertiesResults.count())
	{
		reportHtml += "<ul>";

		for (int i = 0; i < propertyList.count(); i++)
		{
			const bool isTrue = propertiesResults[i] == "true";
			QString truthValue = Qt::escape(propertiesResults[i]);

			if (isTrue)
				truthValue = "<font color=darkgreen>" + truthValue + "</font>";
			else
				truthValue = "<font color=red>" + truthValue + "</font>";

			reportHtml += QString("<li><b>%1</b> is <b>%2</b><pre>%3</pre></li>")
				.arg(Qt::escape(propertyList[i].name()))
				.arg(truthValue)
				.arg(Qt::escape(propertyList[i].expression()));
		}

		reportHtml += "</ul>";
	}
	else
	{
		reportHtml += "<p>Failed to retrieve data</p>";
	}

	m_ui->reportText->setHtml(reportHtml);
}

}
