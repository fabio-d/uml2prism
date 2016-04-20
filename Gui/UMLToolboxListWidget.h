#ifndef GUI_UMLTOOLBOXLISTWIDGET_H
#define GUI_UMLTOOLBOXLISTWIDGET_H

#include <QListWidget>

namespace Gui
{

class UMLToolboxListWidget : public QListWidget
{
	Q_OBJECT

	public:
		explicit UMLToolboxListWidget(QWidget *parent = nullptr);

	protected:
		QStringList mimeTypes() const override;
		QMimeData *mimeData(const QList<QListWidgetItem*> items) const override;

	private:
		void addTool(const QString &text, const QIcon &icon, const QByteArray &mimeData);
};

}

#endif // GUI_UMLTOOLBOXLISTWIDGET_H
