#ifndef GUI_EDITDATATYPENAMEWIDGET_H
#define GUI_EDITDATATYPENAMEWIDGET_H

#include "Core/DatatypeName.h"

#include <QWidget>

class Ui_EditDatatypeNameWidget;

namespace Gui
{

class EditDatatypeNameWidget : public QWidget
{
	Q_OBJECT

	public:
		explicit EditDatatypeNameWidget(QWidget *parent = nullptr);
		~EditDatatypeNameWidget();

		void setExistingDatatypeNamesList(const QStringList &list);

		void setDatatypeName(const Core::DatatypeName &dt);
		Core::DatatypeName datatypeName() const;

	signals:
		void datatypeEdited();

	private slots:
		void slotSomethingChanged();
		void slotOtherRadioButtonToggled(bool checked);

	private:
		void selectOrSetText(const QString &value);

		Ui_EditDatatypeNameWidget *m_ui;
		bool m_ignoreEditSignals;
};

}

#endif // GUI_EDITDATATYPENAMEWIDGET_H
