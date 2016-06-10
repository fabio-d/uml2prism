#ifndef GUI_EDITPREDICATEDIALOG_H
#define GUI_EDITPREDICATEDIALOG_H

#include <QDialog>

class Ui_EditPredicateDialog;

namespace Core
{
class Document;
class Predicate;
enum class PredicateType;
}

namespace Gui
{

class EditPredicateDialog : public QDialog
{
	Q_OBJECT

	public:
		EditPredicateDialog(Core::Document *doc, Core::Predicate *pred, Core::PredicateType type, QWidget *parent = nullptr);
		~EditPredicateDialog();

		void accept() override;

	private slots:
		void slotParse();

	private:
		Ui_EditPredicateDialog *m_ui;
		Core::Document *m_doc;
		Core::Predicate *m_pred;
		Core::PredicateType m_predType;
};

}

#endif // GUI_EDITPREDICATEDIALOG_H
