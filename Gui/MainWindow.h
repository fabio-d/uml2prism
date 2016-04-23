#ifndef GUI_MAINWINDOW_H
#define GUI_MAINWINDOW_H

#include <QMainWindow>

class Ui_MainWindow;

namespace Core
{
class UMLDocument;
};

namespace Gui
{

class UMLGraphicsScene;

class MainWindow : public QMainWindow
{
	Q_OBJECT

	public:
		explicit MainWindow(QWidget *parent = nullptr);
		~MainWindow();

	private slots:
		void slotTabSwitched();

	private:
		Ui_MainWindow *m_ui;

		Core::UMLDocument *m_doc;
		UMLGraphicsScene *m_umlGraphicsScene;
};

}

#endif // GUI_MAINWINDOW_H
