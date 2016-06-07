#ifndef GUI_PREDICATELISTEDITWIDGET_H
#define GUI_PREDICATELISTEDITWIDGET_H

#include <QWidget>

#include "Core/Document.h"

class Ui_PredicateListEditWidget;

namespace Core
{
class PredicateList;
}

namespace Gui
{
class UndoManager;

class PredicateListEditWidget : public QWidget
{
	Q_OBJECT

	friend class PredicateListAddUndoCommand;
	friend class PredicateListEditUndoCommand;
	friend class PredicateListRemoveUndoCommand;
	friend class PredicateListMoveUpUndoCommand;
	friend class PredicateListMoveDownUndoCommand;

	public:
		explicit PredicateListEditWidget(QWidget *parent = nullptr);
		~PredicateListEditWidget();

		void setList(Core::PredicateList *list);
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
		void slotDeserializationCompleted(Core::Document::SerializationOptions loadedWhat);

	private:
		bool eventFilter(QObject *obj, QEvent *event) override;

		void flushChanges();

		Ui_PredicateListEditWidget *m_ui;
		Core::PredicateList *m_docList;
		UndoManager *m_undoManager;
};

}

#endif // GUI_PREDICATELISTEDITWIDGET_H
