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

		Core::UMLDocument *m_activityDoc;
		UMLGraphicsScene *m_umlGraphicsSceneActivity;

		Core::UMLDocument *m_classDoc;
		UMLGraphicsScene *m_umlGraphicsSceneClass;
};

}

#endif // GUI_MAINWINDOW_H
