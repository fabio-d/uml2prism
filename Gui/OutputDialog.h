#ifndef GUI_OUTPUTDIALOG_H
#define GUI_OUTPUTDIALOG_H

#include <QDialog>

class Ui_OutputDialog;

namespace Gui
{

class OutputDialog : public QDialog
{
	Q_OBJECT

	public:
		explicit OutputDialog(const QString &model, const QString &properties, QWidget *parent = nullptr);
		~OutputDialog();

	private:
		Ui_OutputDialog *m_ui;
};

}

#endif // GUI_OUTPUTDIALOG_H
