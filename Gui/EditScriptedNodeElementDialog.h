#ifndef GUI_EDITSCRIPTEDNODEELEMENTDIALOG_H
#define GUI_EDITSCRIPTEDNODEELEMENTDIALOG_H

#include <QDialog>

class Ui_EditScriptedNodeElementDialog;

namespace Core
{
class Document;
class UMLScriptedNodeElement;
}

namespace Gui
{

class EditScriptedNodeElementDialog : public QDialog
{
	Q_OBJECT

	public:
		EditScriptedNodeElementDialog(Core::Document *doc, Core::UMLScriptedNodeElement *elem, QWidget *parent = nullptr);
		~EditScriptedNodeElementDialog();

		void accept() override;

	private slots:
		void slotParse();

	private:
		Ui_EditScriptedNodeElementDialog *m_ui;
		Core::Document *m_doc;
		Core::UMLScriptedNodeElement *m_elem;
};

}

#endif // GUI_EDITSCRIPTEDNODEELEMENTDIALOG_H
