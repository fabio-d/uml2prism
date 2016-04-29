#ifndef GUI_RENAMEDIALOG_H
#define GUI_RENAMEDIALOG_H

#include <QDialog>

class Ui_RenameDialog;

namespace Core
{
class UMLControlFlowEdge;
class UMLDatatypeElement;
class UMLElement;
class UMLNodeElement;
class UMLSignalEdge;
}

namespace Gui
{

class RenameDialog : public QDialog
{
	Q_OBJECT

	public:
		explicit RenameDialog(Core::UMLElement *elem, QWidget *parent = nullptr);
		~RenameDialog();

		void accept() override;

	private slots:
		void slotTextChanged();

	private:
		Ui_RenameDialog *m_ui;
		Core::UMLNodeElement *m_nodeElem;
		Core::UMLControlFlowEdge *m_controlFlowElem;
		Core::UMLSignalEdge *m_signalFlowElem;
		Core::UMLDatatypeElement *m_datatypeElem;
};

}

#endif // GUI_RENAMEDIALOG_H
