#ifndef GUI_MAINWINDOW_H
#define GUI_MAINWINDOW_H

#include <QMainWindow>

class Ui_MainWindow;

namespace Core
{
class Document;
};

namespace Gui
{

class UMLGraphicsScene;
class UndoManager;

class MainWindow : public QMainWindow
{
	Q_OBJECT

	public:
		explicit MainWindow(QWidget *parent = nullptr);
		~MainWindow();

		bool loadFile(const QString &path);

	protected:
		void closeEvent(QCloseEvent *event) override;

	private slots:
		void slotUndoCleanChanged(bool clean);
		void slotNew();
		void slotOpen();
		bool slotSave();
		bool slotSaveAs();
		void slotTabSwitched();
		void slotActionsEnabledChanged(bool editEnabled, bool renameEnabled, bool deleteEnabled);
		void slotRenameItem();
		void slotEditItem();
		void slotDeleteItem();
		void slotFillContextMenu(QMenu *menu);
		void slotZoomIn();
		void slotZoomOut();
		void slotZoomOriginal();
		void slotZoomFit();

	private:
		bool queryClose();
		QString docName() const;

		Ui_MainWindow *m_ui;
		UndoManager *m_undoManager;

		Core::Document *m_doc;
		QString m_filename;

		UMLGraphicsScene *m_umlGraphicsSceneActivity;
		bool m_activityEditEnabled, m_activityRenameEnabled, m_activityDeleteEnabled;
		UMLGraphicsScene *m_umlGraphicsSceneClass;
		bool m_classEditEnabled, m_classRenameEnabled, m_classDeleteEnabled;
};

}

#endif // GUI_MAINWINDOW_H
