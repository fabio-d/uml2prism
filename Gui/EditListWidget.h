#ifndef GUI_EDITLISTWIDGET_H
#define GUI_EDITLISTWIDGET_H

#include <QWidget>

class QListWidgetItem;

class Ui_EditListWidget;

namespace Gui
{

class EditListWidgetCallbacks
{
	public:
		virtual void setEditorData(const QVariant &initialData) = 0;
		virtual void setEditorEnabled(bool enable) = 0;

		virtual QString formatData(const QVariant &data) = 0;
		virtual void formatFont(const QVariant &data, QFont &font);

		virtual QVariant generateEmptyEntry() = 0;
		virtual bool testEntryEmpty(const QVariant &data) = 0;
};

class EditListWidget : public QWidget
{
	Q_OBJECT

	public:
		explicit EditListWidget(QWidget *parent = nullptr);
		~EditListWidget();

		void setCallbacks(EditListWidgetCallbacks *callbacks);

		void setValues(const QList<QVariant> &values);
		QList<QVariant> values() const;

		void setEditedData(const QVariant &data);

	private slots:
		void slotAdd();
		void slotRemove();
		void slotMoveUp();
		void slotMoveDown();
		void slotCurrentRowChanged();

	private:
		void updateFontUnderline(QListWidgetItem *item);

		Ui_EditListWidget *m_ui;
		EditListWidgetCallbacks *m_callbacks;
};

}

#endif // GUI_EDITLISTWIDGET_H
