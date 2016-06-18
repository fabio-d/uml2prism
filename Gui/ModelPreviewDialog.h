#ifndef GUI_MODELPREVIEWDIALOG_H
#define GUI_MODELPREVIEWDIALOG_H

#include <QDialog>

class Ui_ModelPreviewDialog;

namespace Gui
{

class ModelPreviewDialog : public QDialog
{
	Q_OBJECT

	public:
		ModelPreviewDialog(const QString &model, const QString &propertiesPCTL, const QString &propertiesCTL, QWidget *parent = nullptr);
		~ModelPreviewDialog();

	private slots:
		void propTypeChanged();

	private:
		Ui_ModelPreviewDialog *m_ui;
		QString m_nm, m_pctl, m_ctl;
};

}

#endif // GUI_MODELPREVIEWDIALOG_H
