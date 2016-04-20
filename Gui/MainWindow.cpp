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

	m_doc = new Core::UMLDocument();

	m_umlGraphicsScene = new UMLGraphicsScene(m_doc, this);
	m_ui->umlGraphicsView->setScene(m_umlGraphicsScene);
}

MainWindow::~MainWindow()
{
	delete m_doc;

	delete m_ui;
}

}
