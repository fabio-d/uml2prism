#include "Gui/UMLToolboxListWidget.h"

#include <QMimeData>

namespace Gui
{

UMLToolboxListWidget::UMLToolboxListWidget(QWidget *parent)
: QListWidget(parent)
{
	connect(this, SIGNAL(itemClicked(QListWidgetItem*)),
		this, SLOT(slotItemClick(QListWidgetItem*)));
}

QStringList UMLToolboxListWidget::mimeTypes() const
{
	return QStringList()
		<< "application/x-uml-create-flow"
		<< "application/x-uml-create-node";
}

QMimeData *UMLToolboxListWidget::mimeData(const QList<QListWidgetItem*> items) const
{
	if (items.isEmpty())
		return nullptr;

	QMimeData *mimeData = new QMimeData();
	mimeData->setData(
		items[0]->data(Qt::UserRole).toByteArray(),
		items[0]->data(Qt::UserRole + 1).toByteArray());
	return mimeData;
}

void UMLToolboxListWidget::addTool(const QString &text, const QIcon &icon,
	const QByteArray &mimeType, const QByteArray &mimeData)
{
        QListWidgetItem *item = new QListWidgetItem(icon, text, this);
	item->setToolTip(text);
	item->setData(Qt::UserRole, mimeType);
	item->setData(Qt::UserRole + 1, mimeData);
        addItem(item);
}

void UMLToolboxListWidget::slotItemClick(QListWidgetItem *item)
{
	startDrag(Qt::CopyAction);
}

QSize UMLToolboxListWidget::sizeHint() const
{
	return QSize(
		sizeHintForColumn(0),
		sizeHintForRow(0) * count());
}

UMLActivityDiagramToolboxListWidget::UMLActivityDiagramToolboxListWidget(QWidget *parent)
: UMLToolboxListWidget(parent)
{
	addTool("Initial Node",
		QIcon(":/topcased_icons/InitialNode_24.gif"),
		"application/x-uml-create-node", "InitialNode");

	addTool("Action Node",
		QIcon(":/topcased_icons/OpaqueAction_24.gif"),
		"application/x-uml-create-node", "ActionNode");

	addTool("Decision Node",
		QIcon(":/topcased_icons/DecisionNode_24.gif"),
		"application/x-uml-create-node", "DecisionNode");

	addTool("Merge Node",
		QIcon(":/topcased_icons/MergeNode_24.gif"),
		"application/x-uml-create-node", "MergeNode");

	addTool("Fork Node",
		QIcon(":/topcased_icons/ForkNode_24.gif"),
		"application/x-uml-create-node", "ForkNode");

	addTool("Join Node",
		QIcon(":/topcased_icons/JoinNode_24.gif"),
		"application/x-uml-create-node", "JoinNode");

	addTool("Flow Final Node",
		QIcon(":/topcased_icons/FlowFinalNode_24.gif"),
		"application/x-uml-create-node", "FlowFinalNode");

	addTool("Activity Final Node",
		QIcon(":/topcased_icons/ActivityFinalNode_24.gif"),
		"application/x-uml-create-node", "ActivityFinalNode");

	addTool("Control Flow",
		QIcon(":/topcased_icons/ControlFlow_24.gif"),
		"application/x-uml-create-flow", "ControlFlow");

	addTool("Signal / Object Flow",
		QIcon(":/topcased_icons/ObjectFlow_24.gif"),
		"application/x-uml-create-flow", "Signal");
}

UMLClassDiagramToolboxListWidget::UMLClassDiagramToolboxListWidget(QWidget *parent)
: UMLToolboxListWidget(parent)
{
	addTool("Enumeration",
		QIcon(":/topcased_icons/Enumeration_24.gif"),
		"application/x-uml-create-datatype", "Enumeration");

	addTool("Class",
		QIcon(":/topcased_icons/Class_24.gif"),
		"application/x-uml-create-datatype", "Class");

	addTool("Global variables",
		QIcon(":/topcased_icons/GlobalVariables_24.png"),
		"application/x-uml-create-datatype", "GlobalVariables");
}

}
