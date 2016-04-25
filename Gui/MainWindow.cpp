#include "Gui/MainWindow.h"

#include "Gui/UMLGraphicsScene.h"

#include "Core/Document.h"
#include "Core/UMLDiagram.h"

#include <QCloseEvent>
#include <QFileDialog>
#include <QMessageBox>

#include "ui_MainWindow.h"

namespace Gui
{

MainWindow::MainWindow(QWidget *parent)
: QMainWindow(parent), m_ui(new Ui_MainWindow),
  m_activityEditEnabled(false), m_activityDeleteEnabled(false),
  m_classEditEnabled(false), m_classDeleteEnabled(false)
{
	m_ui->setupUi(this);

	m_ui->actionNew->setShortcut(QKeySequence::New);
	m_ui->actionOpen->setShortcut(QKeySequence::Open);
	m_ui->actionSave->setShortcut(QKeySequence::Save);
	m_ui->actionSaveAs->setShortcut(QKeySequence::SaveAs);
	m_ui->actionClose->setShortcut(QKeySequence::Close);
	m_ui->actionQuit->setShortcut(QKeySequence::Quit);
	m_ui->actionZoomIn->setShortcut(QKeySequence::ZoomIn);
	m_ui->actionZoomOut->setShortcut(QKeySequence::ZoomOut);
	m_ui->actionDeleteItem->setShortcut(QKeySequence::Delete);

	connect(m_ui->actionQuit, SIGNAL(triggered()), qApp, SLOT(closeAllWindows()));

	m_doc = new Core::Document();
	m_umlGraphicsSceneActivity = new UMLGraphicsScene(m_doc->activityDiagram(), this);
	connect(m_umlGraphicsSceneActivity, SIGNAL(actionsEnabledChanged(bool, bool)),
		this, SLOT(slotActionsEnabledChanged(bool, bool)));
	connect(m_umlGraphicsSceneActivity, SIGNAL(fillContextMenu(QMenu*)),
		this, SLOT(slotFillContextMenu(QMenu*)));
	m_ui->umlGraphicsViewActivity->setScene(m_umlGraphicsSceneActivity);

	m_umlGraphicsSceneClass = new UMLGraphicsScene(m_doc->classDiagram(), this);
	connect(m_umlGraphicsSceneClass, SIGNAL(actionsEnabledChanged(bool, bool)),
		this, SLOT(slotActionsEnabledChanged(bool, bool)));
	connect(m_umlGraphicsSceneActivity, SIGNAL(fillContextMenu(QMenu*)),
		this, SLOT(slotFillContextMenu(QMenu*)));
	m_ui->umlGraphicsViewClass->setScene(m_umlGraphicsSceneClass);

	slotTabSwitched();
}

MainWindow::~MainWindow()
{
	delete m_doc;
	delete m_ui;
}

void MainWindow::closeEvent(QCloseEvent *event)
{
	if (!queryClose())
		event->ignore();
}

bool MainWindow::queryClose()
{
	// Se non ci sono cambiamenti non salvati
	if (!m_ui->actionSave->isEnabled())
		return true;

	switch (QMessageBox::warning(this, "Close document",
		QString("The document \"%1\" has been modified.\nDo you want to save your changes or discard them?").arg(docName()),
		QMessageBox::Discard | QMessageBox::Save | QMessageBox::Cancel))
	{
		case QMessageBox::Discard:
			return true;
		case QMessageBox::Save:
			return slotSave();
		case QMessageBox::Cancel:
			return false;
		default:
			Q_ASSERT(false && "This should never happen");
			return false;
	}
}

bool MainWindow::loadFile(const QString &path)
{
	QFile file(path);
	bool success;

	if (!file.open(QIODevice::ReadOnly))
	{
		QMessageBox::critical(this, "Open error", "Cannot read file " + path);
		success = false;
	}
	else
	{
		success = m_doc->deserialize(file.readAll());
		m_filename = path;
		// TODO: clear undo history
		if (!success)
			QMessageBox::critical(this, "Open error", "Invalid file");
		file.close();
	}

	if (!success)
	{
		m_doc->clear();
		m_filename = QString();
		// TODO: clear undo history
	}

	return success;
}

QString MainWindow::docName() const
{
	if (m_filename.isEmpty())
		return "Untitled model";

	return QFileInfo(m_filename).fileName();
}

void MainWindow::slotNew()
{
	MainWindow *newWindow = new MainWindow();
	newWindow->show();
}

void MainWindow::slotOpen()
{
	const QString selectedFileName = QFileDialog::getOpenFileName(this,
		"Open document", m_filename, "XML Model (*.xmdl)");

	if (selectedFileName.isEmpty())
		return;

	MainWindow *newWindow = new MainWindow();
	newWindow->show();
	newWindow->loadFile(selectedFileName);
}

bool MainWindow::slotSave()
{
	if (m_filename.isEmpty())
		return slotSaveAs();

	QFile file(m_filename);
	if (!file.open(QIODevice::WriteOnly))
	{
		QMessageBox::critical(this, "Save error", "Cannot write file " + m_filename);
		return false;
	}

	file.write(m_doc->serialize());
	// TODO: mark undo history as clean
	file.close();

	return true;
}

bool MainWindow::slotSaveAs()
{
	const QString selectedFileName = QFileDialog::getSaveFileName(this,
		"Save document", m_filename, "XML Model (*.xmdl)");

	if (selectedFileName.isEmpty())
		return false;

	m_filename = selectedFileName;
	return slotSave();
}

void MainWindow::slotTabSwitched()
{
	if (m_ui->centralTabWidget->currentIndex() == 0)
	{
		m_ui->listWidgetActivityToolbox->setVisible(true);
		m_ui->listWidgetClassToolbox->setVisible(false);
		m_ui->actionRenameItem->setEnabled(m_activityEditEnabled);
		m_ui->actionEditItem->setEnabled(m_activityEditEnabled);
		m_ui->actionDeleteItem->setEnabled(m_activityDeleteEnabled);
	}
	else
	{
		m_ui->listWidgetActivityToolbox->setVisible(false);
		m_ui->listWidgetClassToolbox->setVisible(true);
		m_ui->actionRenameItem->setEnabled(m_classEditEnabled);
		m_ui->actionEditItem->setEnabled(m_classEditEnabled);
		m_ui->actionDeleteItem->setEnabled(m_classDeleteEnabled);
	}
}

void MainWindow::slotActionsEnabledChanged(bool editEnabled, bool deleteEnabled)
{
	if (QObject::sender() == m_umlGraphicsSceneActivity)
	{
		m_activityEditEnabled = editEnabled;
		m_activityDeleteEnabled = deleteEnabled;
	}
	else
	{
		m_classEditEnabled = editEnabled;
		m_classDeleteEnabled = deleteEnabled;
	}

	slotTabSwitched();
}

void MainWindow::slotRenameItem()
{
	if (m_ui->centralTabWidget->currentIndex() == 0)
		m_umlGraphicsSceneActivity->renameSelectedItem();
	else
		m_umlGraphicsSceneClass->renameSelectedItem();
}

void MainWindow::slotEditItem()
{
	if (m_ui->centralTabWidget->currentIndex() == 0)
		m_umlGraphicsSceneActivity->editSelectedItem();
	else
		m_umlGraphicsSceneClass->editSelectedItem();
}

void MainWindow::slotDeleteItem()
{
	if (m_ui->centralTabWidget->currentIndex() == 0)
		m_umlGraphicsSceneActivity->deleteSelectedItems();
	else
		m_umlGraphicsSceneClass->deleteSelectedItems();
}

void MainWindow::slotZoomIn()
{
	if (m_ui->centralTabWidget->currentIndex() == 0)
		m_ui->umlGraphicsViewActivity->zoomIn();
	else
		m_ui->umlGraphicsViewClass->zoomIn();
}

void MainWindow::slotZoomOut()
{
	if (m_ui->centralTabWidget->currentIndex() == 0)
		m_ui->umlGraphicsViewActivity->zoomOut();
	else
		m_ui->umlGraphicsViewClass->zoomOut();
}

void MainWindow::slotZoomOriginal()
{
	if (m_ui->centralTabWidget->currentIndex() == 0)
		m_ui->umlGraphicsViewActivity->zoomOriginal();
	else
		m_ui->umlGraphicsViewClass->zoomOriginal();
}

void MainWindow::slotZoomFit()
{
	if (m_ui->centralTabWidget->currentIndex() == 0)
		m_ui->umlGraphicsViewActivity->zoomFit();
	else
		m_ui->umlGraphicsViewClass->zoomFit();
}

void MainWindow::slotFillContextMenu(QMenu *menu)
{
	if (m_ui->actionRenameItem->isEnabled())
		menu->addAction(m_ui->actionRenameItem);

	if (m_ui->actionEditItem->isEnabled())
		menu->addAction(m_ui->actionRenameItem);

	if (m_ui->actionDeleteItem->isEnabled())
		menu->addAction(m_ui->actionDeleteItem);

}

}
