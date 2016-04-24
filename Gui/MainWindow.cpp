#include "Gui/MainWindow.h"

#include "Gui/UMLGraphicsScene.h"

#include "Core/UMLDiagram.h"

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

	m_activityDoc = new Core::UMLDiagram(Core::UMLDiagram::Activity);
	m_umlGraphicsSceneActivity = new UMLGraphicsScene(m_activityDoc, this);
	connect(m_umlGraphicsSceneActivity, SIGNAL(actionsEnabledChanged(bool, bool)),
		this, SLOT(slotActionsEnabledChanged(bool, bool)));
	connect(m_umlGraphicsSceneActivity, SIGNAL(fillContextMenu(QMenu*)),
		this, SLOT(slotFillContextMenu(QMenu*)));
	m_ui->umlGraphicsViewActivity->setScene(m_umlGraphicsSceneActivity);

	m_classDoc = new Core::UMLDiagram(Core::UMLDiagram::Class);
	m_umlGraphicsSceneClass = new UMLGraphicsScene(m_classDoc, this);
	connect(m_umlGraphicsSceneClass, SIGNAL(actionsEnabledChanged(bool, bool)),
		this, SLOT(slotActionsEnabledChanged(bool, bool)));
	connect(m_umlGraphicsSceneActivity, SIGNAL(fillContextMenu(QMenu*)),
		this, SLOT(slotFillContextMenu(QMenu*)));
	m_ui->umlGraphicsViewClass->setScene(m_umlGraphicsSceneClass);

	slotTabSwitched();
}

MainWindow::~MainWindow()
{
	delete m_classDoc;
	delete m_activityDoc;
	delete m_ui;
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
