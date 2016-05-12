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

	public slots:
		void editSelectedItem();
		void removeSelectedItem();

	private slots:
		void slotAdd();
		void slotMoveUp();
		void slotMoveDown();
		void slotCurrentRowChanged();

	private:
		Ui_PropertyListEditWidget *m_ui;
};

}

#endif // GUI_PROPERTYLISTEDITWIDGET_H
