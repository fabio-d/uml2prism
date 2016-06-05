#ifndef GUI_PROPERTYLISTEDITWIDGET_H
#define GUI_PROPERTYLISTEDITWIDGET_H

#include <QWidget>

class Ui_PropertyListEditWidget;

namespace Core
{
class PropertyList;
}

namespace Gui
{
class UndoManager;

class PropertyListEditWidget : public QWidget
{
	Q_OBJECT

	friend class PropertyListAddUndoCommand;
	friend class PropertyListEditUndoCommand;
	friend class PropertyListRemoveUndoCommand;
	friend class PropertyListMoveUpUndoCommand;
	friend class PropertyListMoveDownUndoCommand;

	public:
		explicit PropertyListEditWidget(QWidget *parent = nullptr);
		~PropertyListEditWidget();

		void setList(Core::PropertyList *list);
		void setUndoManager(UndoManager *undoManager);

	signals:
		void actionsEnabledChanged(bool editEnabled, bool renameEnabled, bool deleteEnabled, bool resetLabelPosEnabled);
		void focusReceived();

	public slots:
		void editSelectedItem();
		void renameSelectedItem();
		void removeSelectedItem();

	private slots:
		void slotAdd();
		void slotMoveUp();
		void slotMoveDown();
		void slotCurrentRowChanged();

	private:
		enum ListType
		{
			LabelList,
			PropertyList
		};

		bool eventFilter(QObject *obj, QEvent *event) override;

		void flushChanges();

		Ui_PropertyListEditWidget *m_ui;
		ListType m_listType;
		Core::PropertyList *m_docList;
		UndoManager *m_undoManager;
};

}

#endif // GUI_PROPERTYLISTEDITWIDGET_H
