#ifndef GUI_EDITCLASSDIALOG_H
#define GUI_EDITCLASSDIALOG_H

#include <QDialog>

#include "Gui/EditListWidget.h"

class Ui_EditClassDialog;

namespace Core
{
class UMLClass;
}

namespace Gui
{

class EditClassDialog : public QDialog, private EditListWidgetCallbacks
{
	Q_OBJECT

	public:
		explicit EditClassDialog(Core::UMLClass *class_, QWidget *parent = nullptr);
		~EditClassDialog();

		void accept() override;

	private slots:
		void slotEdited();

	private:
		void setEditorData(const QVariant &initialData) override;
		void setEditorEnabled(bool enable) override;
		QString formatData(const QVariant &data) override;
		QVariant generateEmptyEntry() override;
		bool testEntryEmpty(const QVariant &data) override;

		Ui_EditClassDialog *m_ui;
		Core::UMLClass *m_class;
};

}

#endif // GUI_EDITCLASSDIALOG_H
