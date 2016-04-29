#ifndef GUI_EDITGLOBALVARIABLESDIALOG_H
#define GUI_EDITGLOBALVARIABLESDIALOG_H

#include <QDialog>

#include "Gui/EditListWidget.h"

class Ui_EditGlobalVariablesDialog;

namespace Core
{
class UMLGlobalVariables;
}

namespace Gui
{

class EditGlobalVariablesDialog : public QDialog, private EditListWidgetCallbacks
{
	Q_OBJECT

	public:
		explicit EditGlobalVariablesDialog(Core::UMLGlobalVariables *globalVariables, QWidget *parent = nullptr);
		~EditGlobalVariablesDialog();

		void accept() override;

	private slots:
		void slotEdited();

	private:
		void setEditorData(const QVariant &initialData) override;
		void setEditorEnabled(bool enable) override;
		QString formatData(const QVariant &data) override;
		void formatFont(const QVariant &data, QFont &font) override;
		QVariant generateEmptyEntry() override;
		bool testEntryEmpty(const QVariant &data) override;

		Ui_EditGlobalVariablesDialog *m_ui;
		Core::UMLGlobalVariables *m_globalVariables;
		bool m_ignoreEditSignals;
};

}

#endif // GUI_EDITGLOBALVARIABLESDIALOG_H
