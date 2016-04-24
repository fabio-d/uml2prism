#include "Gui/MainWindow.h"

#include "Gui/UMLGraphicsScene.h"

#include "Core/UMLDiagram.h"

#include "ui_MainWindow.h"

namespace Gui
{

MainWindow::MainWindow(QWidget *parent)
: QMainWindow(parent), m_ui(new Ui_MainWindow)
{
	m_ui->setupUi(this);

	m_ui->actionNew->setShortcut(QKeySequence::New);
	m_ui->actionOpen->setShortcut(QKeySequence::Open);
	m_ui->actionSave->setShortcut(QKeySequence::Save);
	m_ui->actionSaveAs->setShortcut(QKeySequence::SaveAs);
	m_ui->actionClose->setShortcut(QKeySequence::Close);
	m_ui->actionQuit->setShortcut(QKeySequence::Quit);

	m_activityDoc = new Core::UMLDiagram(Core::UMLDiagram::Activity);
	m_umlGraphicsSceneActivity = new UMLGraphicsScene(m_activityDoc, this);
	m_ui->umlGraphicsViewActivity->setScene(m_umlGraphicsSceneActivity);

	m_classDoc = new Core::UMLDiagram(Core::UMLDiagram::Class);
	m_umlGraphicsSceneClass = new UMLGraphicsScene(m_classDoc, this);
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
	m_ui->menuEdit->clear();
	m_ui->toolBarEdit->clear();
	m_ui->menuView->clear();
	m_ui->toolBarView->clear();

	if (m_ui->centralTabWidget->currentIndex() == 0)
	{
		m_ui->listWidgetActivityToolbox->setVisible(true);
		m_ui->listWidgetClassToolbox->setVisible(false);

		m_umlGraphicsSceneActivity->appendEditActions(m_ui->menuEdit);
		m_umlGraphicsSceneActivity->appendEditActions(m_ui->toolBarEdit);
		m_ui->umlGraphicsViewActivity->appendViewActions(m_ui->menuView);
		m_ui->umlGraphicsViewActivity->appendViewActions(m_ui->toolBarView);
	}
	else
	{
		m_ui->listWidgetActivityToolbox->setVisible(false);
		m_ui->listWidgetClassToolbox->setVisible(true);

		m_umlGraphicsSceneClass->appendEditActions(m_ui->menuEdit);
		m_umlGraphicsSceneClass->appendEditActions(m_ui->toolBarEdit);
		m_ui->umlGraphicsViewClass->appendViewActions(m_ui->menuView);
		m_ui->umlGraphicsViewClass->appendViewActions(m_ui->toolBarView);
	}
}

}
