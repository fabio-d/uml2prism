#include "Gui/MainWindow.h"

#include "Gui/UMLGraphicsScene.h"
#include "Gui/UndoManager.h"

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
	m_ui->actionUndo->setShortcut(QKeySequence::Undo);
	m_ui->actionRedo->setShortcut(QKeySequence::Redo);
	m_ui->actionDeleteItem->setShortcut(QKeySequence::Delete);
	m_ui->actionZoomIn->setShortcut(QKeySequence::ZoomIn);
	m_ui->actionZoomOut->setShortcut(QKeySequence::ZoomOut);

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
	connect(m_umlGraphicsSceneClass, SIGNAL(fillContextMenu(QMenu*)),
		this, SLOT(slotFillContextMenu(QMenu*)));
	m_ui->umlGraphicsViewClass->setScene(m_umlGraphicsSceneClass);

	m_undoManager = new UndoManager(m_doc, m_ui->actionUndo, m_ui->actionRedo);
	connect(m_umlGraphicsSceneActivity, SIGNAL(undoCheckpointCreationRequest()),
		m_undoManager, SLOT(createCheckpoint()));
	connect(m_umlGraphicsSceneClass, SIGNAL(undoCheckpointCreationRequest()),
		m_undoManager, SLOT(createCheckpoint()));
	connect(m_undoManager, SIGNAL(undoCleanChanged(bool)),
		this, SLOT(slotUndoCleanChanged(bool)));
	slotUndoCleanChanged(true);

	slotTabSwitched();
}

MainWindow::~MainWindow()
{
	delete m_undoManager;
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
		m_undoManager->clearStack();
		if (!success)
			QMessageBox::critical(this, "Open error", "Invalid file");
		file.close();
	}

	if (!success)
	{
		m_doc->clear();
		m_filename = QString();
		m_undoManager->clearStack();
	}

	// Center view and reset zoom
	m_ui->umlGraphicsViewActivity->zoomFit();
	m_ui->umlGraphicsViewActivity->zoomOriginal();
	m_ui->umlGraphicsViewClass->zoomFit();
	m_ui->umlGraphicsViewClass->zoomOriginal();

	return success;
}

QString MainWindow::docName() const
{
	if (m_filename.isEmpty())
		return "Untitled model";

	return QFileInfo(m_filename).fileName();
}

void MainWindow::slotUndoCleanChanged(bool clean)
{
	QString title = docName();

	if (!clean)
		title += " [modified]";

	setWindowTitle(title + " - Model Editor");

	m_ui->actionSave->setEnabled(!clean);
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

	MainWindow *targetWindow;

	if (m_filename.isEmpty() && m_undoManager->isStackEmpty())
	{
		targetWindow = this;
	}
	else
	{
		targetWindow = new MainWindow();
		targetWindow->show();
	}

	targetWindow->loadFile(selectedFileName);
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
	m_undoManager->setCleanStack();
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
		m_umlGraphicsSceneActivity->renameSelectedItem(this);
	else
		m_umlGraphicsSceneClass->renameSelectedItem(this);
}

void MainWindow::slotEditItem()
{
	if (m_ui->centralTabWidget->currentIndex() == 0)
		m_umlGraphicsSceneActivity->editSelectedItem(this);
	else
		m_umlGraphicsSceneClass->editSelectedItem(this);
}

void MainWindow::slotDeleteItem()
{
	if (m_ui->centralTabWidget->currentIndex() == 0)
		m_umlGraphicsSceneActivity->deleteSelectedItems(this);
	else
		m_umlGraphicsSceneClass->deleteSelectedItems(this);
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
	if (m_ui->actionEditItem->isEnabled())
		menu->addAction(m_ui->actionEditItem);

	if (m_ui->actionRenameItem->isEnabled())
		menu->addAction(m_ui->actionRenameItem);

	if (m_ui->actionDeleteItem->isEnabled())
		menu->addAction(m_ui->actionDeleteItem);

}

}
