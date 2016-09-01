/*
 * Copyright (C) 2016 Fabio D'Urso
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

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
		void showEvent(QShowEvent *event) override;

	private slots:
		void slotUndoCleanChanged(bool clean);
		void slotNew();
		void slotOpen();
		bool slotSave();
		bool slotSaveAs();
		void slotExportSvg();
		void slotTabSwitched();
		void slotActionsEnabledChanged(bool editEnabled, bool renameEnabled, bool deleteEnabled, bool resetLabelPosEnabled);
		void slotRenameItem();
		void slotEditItem();
		void slotDeleteItem();
		void slotResetLabelPosition();
		void slotFillContextMenu(QMenu *menu);
		void slotZoomIn();
		void slotZoomOut();
		void slotZoomOriginal();
		void slotZoomFit();
		void slotBuild();
		void slotRun();
		void slotWarning(const QString &location, const QString &description);
		void slotError(const QString &location, const QString &description);
		void slotLabelListFocused();
		void slotPropertyListFocused();

	private:
		bool queryClose();
		QString docName() const;
		bool buildModel(QString *out_nm, QString *out_pctl, QString *out_ctl);

		Ui_MainWindow *m_ui;
		UndoManager *m_undoManager;

		Core::Document *m_doc;
		QString m_filename;

		UMLGraphicsScene *m_umlGraphicsSceneActivity;
		bool m_activityEditEnabled, m_activityRenameEnabled, m_activityDeleteEnabled;
		bool m_activityResetLabelPositionEnabled;

		UMLGraphicsScene *m_umlGraphicsSceneClass;
		bool m_classEditEnabled, m_classRenameEnabled, m_classDeleteEnabled;

		bool m_labelEditEnabled, m_labelDeleteEnabled;
		bool m_propertyEditEnabled, m_propertyDeleteEnabled;

		// Have these tabs already been shown?
		bool m_activityDiagramFirstShown, m_classDiagramFirstShown;

		// Which list has been focused last in the properties tab?
		enum { LabelList, PropertyList } m_lastFocusedList;
};

}

#endif // GUI_MAINWINDOW_H
