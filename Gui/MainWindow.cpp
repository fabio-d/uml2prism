#include "MainWindow.h"

#include "UMLGraphicsScene.h"

#include "ui_MainWindow.h"

namespace Gui
{

MainWindow::MainWindow(QWidget *parent)
: QMainWindow(parent), m_ui(new Ui_MainWindow)
{
	m_ui->setupUi(this);

	m_umlGraphicsScene = new UMLGraphicsScene(this);
	m_ui->umlGraphicsView->setScene(m_umlGraphicsScene);
}

MainWindow::~MainWindow()
{
	delete m_ui;
}

}
