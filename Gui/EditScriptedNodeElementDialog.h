#ifndef GUI_EDITSCRIPTEDNODEELEMENTDIALOG_H
#define GUI_EDITSCRIPTEDNODEELEMENTDIALOG_H

#include <QDialog>

class Ui_EditScriptedNodeElementDialog;

namespace Core
{
class UMLScriptedNodeElement;
}

namespace Gui
{

class EditScriptedNodeElementDialog : public QDialog
{
	Q_OBJECT

	public:
		explicit EditScriptedNodeElementDialog(Core::UMLScriptedNodeElement *elem, QWidget *parent = nullptr);
		~EditScriptedNodeElementDialog();

		void accept() override;

	private slots:
		void slotParse();

	private:
		Ui_EditScriptedNodeElementDialog *m_ui;
		Core::UMLScriptedNodeElement *m_elem;
};

}

#endif // GUI_EDITSCRIPTEDNODEELEMENTDIALOG_H
