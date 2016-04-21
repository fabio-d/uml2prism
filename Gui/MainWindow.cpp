#include "Gui/MainWindow.h"

#include "Gui/UMLGraphicsScene.h"

#include "Core/UMLDocument.h"

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

	m_doc = new Core::UMLDocument();

	m_umlGraphicsScene = new UMLGraphicsScene(m_doc, this);
	m_ui->umlGraphicsView->setScene(m_umlGraphicsScene);

	m_umlGraphicsScene->addActions(m_ui->menuEdit);
	m_umlGraphicsScene->addActions(m_ui->toolBar);
}

MainWindow::~MainWindow()
{
	delete m_doc;

	delete m_ui;
}

}
