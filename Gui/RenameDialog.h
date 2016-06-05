#ifndef GUI_RENAMEDIALOG_H
#define GUI_RENAMEDIALOG_H

#include <QDialog>

class Ui_RenameDialog;

namespace Core
{
class Predicate;
enum class PredicateType;
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
		RenameDialog(Core::Predicate *pred, Core::PredicateType type, QWidget *parent = nullptr);
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
		Core::Predicate *m_pred;
		Core::PredicateType m_predType;
};

}

#endif // GUI_RENAMEDIALOG_H
