#include "UMLToolboxListWidget.h"

#include <QMimeData>

namespace Gui
{

UMLToolboxListWidget::UMLToolboxListWidget(QWidget *parent)
: QListWidget(parent)
{
	addTool("Initial Node",
		QIcon(":/topcased_icons/resources/topcased_icons/InitialNode_24.gif"),
		"InitialNode");

	addTool("Decision Node",
		QIcon(":/topcased_icons/resources/topcased_icons/DecisionNode_24.gif"),
		"DecisionNode");

	addTool("Merge Node",
		QIcon(":/topcased_icons/resources/topcased_icons/MergeNode_24.gif"),
		"MergeNode");

	addTool("Fork Node",
		QIcon(":/topcased_icons/resources/topcased_icons/ForkNode_24.gif"),
		"ForkNode");

	addTool("Join Node",
		QIcon(":/topcased_icons/resources/topcased_icons/JoinNode_24.gif"),
		"JoinNode");

	addTool("Final Node",
		QIcon(":/topcased_icons/resources/topcased_icons/ActivityFinalNode_24.gif"),
		"FinalNode");

	addTool("Control Flow",
		QIcon(":/topcased_icons/resources/topcased_icons/ControlFlow_24.gif"),
		"ControlFlow");

	addTool("Signal / Object Flow",
		QIcon(":/topcased_icons/resources/topcased_icons/ObjectFlow_24.gif"),
		"ObjectFlow");

	addTool("Input Pin",
		QIcon(":/topcased_icons/resources/topcased_icons/InputPin_24.gif"),
		"InputPin");

	addTool("Output Pin",
		QIcon(":/topcased_icons/resources/topcased_icons/OutputPin_24.gif"),
		"OutputPin");
}

QStringList UMLToolboxListWidget::mimeTypes() const
{
	return QStringList() << "application/x-uml-create-element";
}

QMimeData *UMLToolboxListWidget::mimeData(const QList<QListWidgetItem*> items) const
{
	if (items.isEmpty())
		return nullptr;

	QMimeData *mimeData = new QMimeData();
	mimeData->setData("application/x-uml-create-element", items[0]->data(Qt::UserRole).toByteArray());
	return mimeData;
}

void UMLToolboxListWidget::addTool(const QString &text, const QIcon &icon, const QByteArray &mimeData)
{
        QListWidgetItem *item = new QListWidgetItem(icon, text, this);
	item->setData(Qt::UserRole, mimeData);
        addItem(item);
}

}
