#ifndef GUI_MAINWINDOW_H
#define GUI_MAINWINDOW_H

#include <QMainWindow>

class Ui_MainWindow;

namespace Core
{
class UMLDiagram;
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
		void slotActionsEnabledChanged(bool editEnabled, bool deleteEnabled);
		void slotRenameItem();
		void slotEditItem();
		void slotDeleteItem();
		void slotFillContextMenu(QMenu *menu);
		void slotZoomIn();
		void slotZoomOut();
		void slotZoomOriginal();
		void slotZoomFit();

	private:
		Ui_MainWindow *m_ui;

		Core::UMLDiagram *m_activityDoc;
		UMLGraphicsScene *m_umlGraphicsSceneActivity;
		bool m_activityEditEnabled, m_activityDeleteEnabled;

		Core::UMLDiagram *m_classDoc;
		UMLGraphicsScene *m_umlGraphicsSceneClass;
		bool m_classEditEnabled, m_classDeleteEnabled;
};

}

#endif // GUI_MAINWINDOW_H
