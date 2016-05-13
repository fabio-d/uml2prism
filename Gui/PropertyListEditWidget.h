#ifndef GUI_PROPERTYLISTEDITWIDGET_H
#define GUI_PROPERTYLISTEDITWIDGET_H

#include "Gui/EditListWidget.h"

class Ui_PropertyListEditWidget;

namespace Gui
{

class PropertyListEditWidget : public QWidget //, private EditListWidgetCallbacks
{
	Q_OBJECT

	public:
		explicit PropertyListEditWidget(QWidget *parent = nullptr);
		~PropertyListEditWidget();

		bool eventFilter(QObject *obj, QEvent *event) override;

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
		void updateActions();

		Ui_PropertyListEditWidget *m_ui;
};

}

#endif // GUI_PROPERTYLISTEDITWIDGET_H
