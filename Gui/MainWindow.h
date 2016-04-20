#ifndef GUI_MAINWINDOW_H
#define GUI_MAINWINDOW_H

#include <QMainWindow>

class Ui_MainWindow;

namespace Gui
{

class UMLGraphicsScene;

class MainWindow : public QMainWindow
{
	Q_OBJECT

	public:
		explicit MainWindow(QWidget *parent = nullptr);
		~MainWindow();

	private:
		Ui_MainWindow *m_ui;
		UMLGraphicsScene *m_umlGraphicsScene;
};

}

#endif // GUI_MAINWINDOW_H
