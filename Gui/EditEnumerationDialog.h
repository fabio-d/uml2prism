#ifndef GUI_EDITENUMERATIONDIALOG_H
#define GUI_EDITENUMERATIONDIALOG_H

#include <QDialog>

#include "Gui/EditListWidget.h"

class Ui_EditEnumerationDialog;

namespace Core
{
class UMLEnumeration;
}

namespace Gui
{

class EditEnumerationDialog : public QDialog, private EditListWidgetCallbacks
{
	Q_OBJECT

	public:
		explicit EditEnumerationDialog(Core::UMLEnumeration *enumeration, QWidget *parent = nullptr);
		~EditEnumerationDialog();

		void accept() override;

	private slots:
		void slotEdited();

	private:
		void setEditorData(const QVariant &initialData) override;
		void setEditorEnabled(bool enable) override;
		QString formatData(const QVariant &data) override;
		QVariant generateEmptyEntry() override;
		bool testEntryEmpty(const QVariant &data) override;

		Ui_EditEnumerationDialog *m_ui;
		Core::UMLEnumeration *m_enumeration;
};

}

#endif // GUI_EDITENUMERATIONDIALOG_H
