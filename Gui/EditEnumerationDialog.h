#ifndef GUI_EDITENUMERATIONDIALOG_H
#define GUI_EDITENUMERATIONDIALOG_H

#include <QDialog>

class Ui_EditEnumerationDialog;

namespace Core
{
class UMLEnumeration;
}

namespace Gui
{

class EditEnumerationDialog : public QDialog
{
	Q_OBJECT

	public:
		explicit EditEnumerationDialog(Core::UMLEnumeration *enumeration, QWidget *parent = nullptr);
		~EditEnumerationDialog();

		void accept() override;

	private slots:
		void slotAdd();
		void slotRemove();
		void slotMoveUp();
		void slotMoveDown();
		void slotSelectedRowChanged();
		void slotEdited();

	private:
		Ui_EditEnumerationDialog *m_ui;
		Core::UMLEnumeration *m_enumeration;
};

}

#endif // GUI_EDITENUMERATIONDIALOG_H
