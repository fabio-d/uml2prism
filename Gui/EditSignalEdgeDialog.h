#ifndef GUI_EDITSIGNALEDGEDIALOG_H
#define GUI_EDITSIGNALEDGEDIALOG_H

#include <QDialog>

class Ui_EditSignalEdgeDialog;

namespace Core
{
class UMLSignalEdge;
}

namespace Gui
{

class EditSignalEdgeDialog : public QDialog
{
	Q_OBJECT

	public:
		explicit EditSignalEdgeDialog(Core::UMLSignalEdge *signalEdge, QWidget *parent = nullptr);
		~EditSignalEdgeDialog();

		void accept() override;

	private:
		Ui_EditSignalEdgeDialog *m_ui;
		Core::UMLSignalEdge *m_signalEdge;
};

}

#endif // GUI_EDITSIGNALEDGEDIALOG_H
