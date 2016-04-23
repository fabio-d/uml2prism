#ifndef GUI_UMLTOOLBOXLISTWIDGET_H
#define GUI_UMLTOOLBOXLISTWIDGET_H

#include <QListWidget>

namespace Gui
{

class UMLToolboxListWidget : public QListWidget
{
	Q_OBJECT

	protected:
		explicit UMLToolboxListWidget(QWidget *parent = nullptr);

		QStringList mimeTypes() const override;
		QMimeData *mimeData(const QList<QListWidgetItem*> items) const override;

		void addTool(const QString &text, const QIcon &icon,
			const QByteArray &mimeType, const QByteArray &mimeData);

	private slots:
		void slotItemClick(QListWidgetItem *item);

	private:
		QSize sizeHint() const override;
};

class UMLActivityDiagramToolboxListWidget : public UMLToolboxListWidget
{
	public:
		explicit UMLActivityDiagramToolboxListWidget(QWidget *parent = nullptr);
};

class UMLClassDiagramToolboxListWidget : public UMLToolboxListWidget
{
	public:
		explicit UMLClassDiagramToolboxListWidget(QWidget *parent = nullptr);
};

}

#endif // GUI_UMLTOOLBOXLISTWIDGET_H
