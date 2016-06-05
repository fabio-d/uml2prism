#include "Gui/PredicateListEditWidget.h"

#include "Gui/UndoManager.h"

#include "Core/Document.h"
#include "Core/PredicateList.h"

#include <QDebug>
#include <QEvent>

#include "ui_PredicateListEditWidget.h"

namespace Gui
{

class PredicateListAddUndoCommand : public QUndoCommand
{
	public:
		PredicateListAddUndoCommand(PredicateListEditWidget *target,
			const QString &name, const QString &value)
		: m_target(target), m_treeWidget(target->m_ui->treeWidget),
		  m_name(name), m_value(value),
		  m_index(m_treeWidget->topLevelItemCount())
		{
		}

		void redo() override
		{
			QTreeWidgetItem *item = new QTreeWidgetItem();
			item->setText(0, m_name);
			item->setText(1, m_value);
			m_treeWidget->insertTopLevelItem(m_index, item);
			m_treeWidget->resizeColumnToContents(0);
			m_target->flushChanges();
		}

		void undo() override
		{
			delete m_treeWidget->takeTopLevelItem(m_index);
			m_target->flushChanges();
		}

	private:
		PredicateListEditWidget *m_target;
		QTreeWidget *m_treeWidget;
		QString m_name, m_value;
		int m_index;
};

class PredicateListEditUndoCommand : public QUndoCommand
{
	public:
		PredicateListEditUndoCommand(PredicateListEditWidget *target, int index,
			const QString &oldName, const QString &oldValue,
			const QString &newName, const QString &newValue)
		: m_target(target), m_treeWidget(target->m_ui->treeWidget),
		  m_index(index), m_oldName(oldName), m_oldValue(oldValue),
		  m_newName(newName), m_newValue(newValue)
		{
		}

		void redo() override
		{
			QTreeWidgetItem *item = m_treeWidget->topLevelItem(m_index);
			item->setText(0, m_newName);
			item->setText(1, m_newValue);
			m_treeWidget->resizeColumnToContents(0);
			m_target->flushChanges();
		}

		void undo() override
		{
			QTreeWidgetItem *item = m_treeWidget->topLevelItem(m_index);
			item->setText(0, m_oldName);
			item->setText(1, m_oldValue);
			m_treeWidget->resizeColumnToContents(0);
			m_target->flushChanges();
		}

	private:
		PredicateListEditWidget *m_target;
		QTreeWidget *m_treeWidget;
		int m_index;
		QString m_oldName, m_oldValue, m_newName, m_newValue;
};

class PredicateListRemoveUndoCommand : public QUndoCommand
{
	public:
		PredicateListRemoveUndoCommand(PredicateListEditWidget *target, int index,
			const QString &name, const QString &value)
		: m_target(target), m_treeWidget(target->m_ui->treeWidget),
		  m_index(index), m_name(name), m_value(value)
		{
		}

		void redo() override
		{
			delete m_treeWidget->takeTopLevelItem(m_index);
			m_target->flushChanges();
		}

		void undo() override
		{
			QTreeWidgetItem *item = new QTreeWidgetItem();
			item->setText(0, m_name);
			item->setText(1, m_value);
			m_treeWidget->insertTopLevelItem(m_index, item);
			m_treeWidget->resizeColumnToContents(0);
			m_target->flushChanges();
		}

	private:
		PredicateListEditWidget *m_target;
		QTreeWidget *m_treeWidget;
		int m_index;
		QString m_name, m_value;
};

class PredicateListMoveUpUndoCommand : public QUndoCommand
{
	public:
		PredicateListMoveUpUndoCommand(PredicateListEditWidget *target, int index)
		: m_target(target), m_treeWidget(target->m_ui->treeWidget),
		  m_index(index)
		{
		}

		void redo() override
		{
			QTreeWidgetItem *item = m_treeWidget->takeTopLevelItem(m_index - 1);
			m_treeWidget->insertTopLevelItem(m_index, item);
			m_target->flushChanges();
		}

		void undo() override
		{
			QTreeWidgetItem *item = m_treeWidget->takeTopLevelItem(m_index);
			m_treeWidget->insertTopLevelItem(m_index - 1, item);
			m_target->flushChanges();
		}

	private:
		PredicateListEditWidget *m_target;
		QTreeWidget *m_treeWidget;
		int m_index;
		QString m_name, m_value;
};

class PredicateListMoveDownUndoCommand : public QUndoCommand
{
	public:
		PredicateListMoveDownUndoCommand(PredicateListEditWidget *target, int index)
		: m_target(target), m_treeWidget(target->m_ui->treeWidget),
		  m_index(index)
		{
		}

		void redo() override
		{
			QTreeWidgetItem *item = m_treeWidget->takeTopLevelItem(m_index + 1);
			m_treeWidget->insertTopLevelItem(m_index, item);
			m_target->flushChanges();
		}

		void undo() override
		{
			QTreeWidgetItem *item = m_treeWidget->takeTopLevelItem(m_index);
			m_treeWidget->insertTopLevelItem(m_index + 1, item);
			m_target->flushChanges();
		}

	private:
		PredicateListEditWidget *m_target;
		QTreeWidget *m_treeWidget;
		int m_index;
		QString m_name, m_value;
};

PredicateListEditWidget::PredicateListEditWidget(QWidget *parent)
: QWidget(parent), m_ui(new Ui_PredicateListEditWidget),
  m_listType(PredicateList), m_docList(nullptr), m_undoManager(nullptr)
{
	m_ui->setupUi(this);
	m_ui->treeWidget->header()->resizeSections(QHeaderView::ResizeToContents);
	m_ui->treeWidget->installEventFilter(this);

	flushChanges();
}

PredicateListEditWidget::~PredicateListEditWidget()
{
	m_ui->treeWidget->removeEventFilter(this);
	delete m_ui;
}

void PredicateListEditWidget::setList(Core::PredicateList *list)
{
	if (list->contentType() == Core::PredicateList::Properties)
		m_listType = PredicateList;
	else
		m_listType = LabelList;

	m_docList = list;
}

void PredicateListEditWidget::setUndoManager(UndoManager *undoManager)
{
	m_undoManager = undoManager;
}

bool PredicateListEditWidget::eventFilter(QObject *obj, QEvent *event)
{
	if (obj == m_ui->treeWidget)
	{
		switch (event->type())
		{
			case QEvent::FocusIn:
				emit focusReceived();
				return QWidget::eventFilter(obj, event);
			default:
				break;
		}
	}

	return QWidget::eventFilter(obj, event);
}

void PredicateListEditWidget::slotAdd()
{
	emit focusReceived();

	Q_ASSERT(m_undoManager != nullptr);

	QString prefix = (m_listType == LabelList) ? "NewLabel" : "NewProperty";

	m_undoManager->push(new PredicateListAddUndoCommand(this,
		m_docList->document()->generateFreshName(prefix),
		"true"));

	m_ui->treeWidget->setCurrentItem(m_ui->treeWidget->topLevelItem(m_ui->treeWidget->topLevelItemCount() - 1));
}

void PredicateListEditWidget::editSelectedItem()
{
	emit focusReceived();
	qDebug() << "Edit";

	if (m_ui->treeWidget->currentItem() == nullptr)
		return;

	Q_ASSERT(m_undoManager != nullptr);
	m_undoManager->push(new PredicateListEditUndoCommand(this,
		m_ui->treeWidget->indexOfTopLevelItem(m_ui->treeWidget->currentItem()),
		m_ui->treeWidget->currentItem()->text(0),
		m_ui->treeWidget->currentItem()->text(1),
		m_ui->treeWidget->currentItem()->text(0) + "~",
		m_ui->treeWidget->currentItem()->text(1) + "~"));
}

void PredicateListEditWidget::renameSelectedItem()
{
	emit focusReceived();
	qDebug() << "Rename";
}

void PredicateListEditWidget::removeSelectedItem()
{
	emit focusReceived();

	if (m_ui->treeWidget->currentItem() == nullptr)
		return;

	Q_ASSERT(m_undoManager != nullptr);
	m_undoManager->push(new PredicateListRemoveUndoCommand(this,
		m_ui->treeWidget->indexOfTopLevelItem(m_ui->treeWidget->currentItem()),
		m_ui->treeWidget->currentItem()->text(0),
		m_ui->treeWidget->currentItem()->text(1)));
}

void PredicateListEditWidget::slotMoveUp()
{
	emit focusReceived();

	if (m_ui->treeWidget->currentItem() == nullptr)
		return;

	const int index = m_ui->treeWidget->indexOfTopLevelItem(m_ui->treeWidget->currentItem());
	if (index == 0)
		return;

	Q_ASSERT(m_undoManager != nullptr);
	m_undoManager->push(new PredicateListMoveUpUndoCommand(this, index));

	m_ui->treeWidget->scrollToItem(m_ui->treeWidget->currentItem());
}

void PredicateListEditWidget::slotMoveDown()
{
	emit focusReceived();

	if (m_ui->treeWidget->currentItem() == nullptr)
		return;

	const int index = m_ui->treeWidget->indexOfTopLevelItem(m_ui->treeWidget->currentItem());
	if (index == m_ui->treeWidget->topLevelItemCount() - 1)
		return;

	Q_ASSERT(m_undoManager != nullptr);
	m_undoManager->push(new PredicateListMoveDownUndoCommand(this, index));

	m_ui->treeWidget->scrollToItem(m_ui->treeWidget->currentItem());
}

void PredicateListEditWidget::slotCurrentRowChanged()
{
	emit focusReceived();
	flushChanges();
}

void PredicateListEditWidget::flushChanges()
{
	const bool nullItem = m_ui->treeWidget->currentItem() == nullptr;
	emit actionsEnabledChanged(!nullItem, !nullItem, !nullItem, false);

	if (nullItem)
	{
		m_ui->editPushButton->setEnabled(false);
		m_ui->removePushButton->setEnabled(false);
		m_ui->moveUpPushButton->setEnabled(false);
		m_ui->moveDownPushButton->setEnabled(false);
	}
	else
	{
		const int index = m_ui->treeWidget->indexOfTopLevelItem(m_ui->treeWidget->currentItem());
		m_ui->editPushButton->setEnabled(true);
		m_ui->removePushButton->setEnabled(true);
		m_ui->moveUpPushButton->setEnabled(index != 0);
		m_ui->moveDownPushButton->setEnabled(index != m_ui->treeWidget->topLevelItemCount() - 1);
	}

	// Write list back into the parent Document 
	if (!m_docList)
		return;

	m_docList->clear();
	for (int i = 0; i < m_ui->treeWidget->topLevelItemCount(); i++)
	{
		QTreeWidgetItem *item = m_ui->treeWidget->topLevelItem(i);
		m_docList->append(item->text(0), item->text(1));
	}
}

}
