#include "Gui/UndoManager.h"

#include "Core/Document.h"

#include <QAction>
#include <QDebug>

namespace Gui
{

class UndoCommand : public QUndoCommand
{
	public:
		UndoCommand(Core::Document *document, const QByteArray &oldXml, const QByteArray &newXml)
		: document(document), initialRedoDone(false), oldXml(oldXml), newXml(newXml)
		{
		}

		void undo() override
		{
			document->deserialize(oldXml);
		}

		void redo() override
		{
			/* Do not apply the command when we are called for the
			 * first time as a result of QUndoStack's push(),
			 * because the current state is already newXml */
			if (!initialRedoDone)
			{
				initialRedoDone = true;
				return;
			}

			document->deserialize(newXml);
		}

	private:
		Core::Document *document;
		bool initialRedoDone;
		QByteArray oldXml, newXml;
};

UndoManager::UndoManager(Core::Document *document, QAction *undoAction, QAction *redoAction, QObject *parent)
: QObject(parent), m_doc(document)
{
	connect(&m_undoStack, SIGNAL(cleanChanged(bool)), this, SIGNAL(undoCleanChanged(bool)));
	connect(&m_undoStack, SIGNAL(canUndoChanged(bool)), undoAction, SLOT(setEnabled(bool)));
	connect(&m_undoStack, SIGNAL(canRedoChanged(bool)), redoAction, SLOT(setEnabled(bool)));

	undoAction->setEnabled(m_undoStack.canUndo());
	redoAction->setEnabled(m_undoStack.canRedo());

	connect(document, SIGNAL(deserializationCompleted()), this, SLOT(updateSavedState()));
	connect(undoAction, SIGNAL(triggered()), &m_undoStack, SLOT(undo()));
	connect(redoAction, SIGNAL(triggered()), &m_undoStack, SLOT(redo()));

	m_prevDocXml = m_doc->serialize();
}

void UndoManager::clearStack()
{
	m_undoStack.clear();
	emit undoCleanChanged(true);
}

void UndoManager::setCleanStack()
{
	m_undoStack.setClean();
	emit undoCleanChanged(true);
}

bool UndoManager::isStackEmpty() const
{
	return m_undoStack.count() == 0;
}

void UndoManager::createCheckpoint()
{
	const QByteArray newDocXml = m_doc->serialize();

	if (m_prevDocXml == newDocXml)
	{
		//qDebug() << "UNDO-SKIP";
		return;
	}

	UndoCommand *cmd = new UndoCommand(m_doc, m_prevDocXml, newDocXml);
	//qDebug() << "UNDO-OLD" << m_prevDocXml;
	//qDebug() << "UNDO-NEW" << newDocXml;
	m_prevDocXml = newDocXml;
	m_undoStack.push(cmd);
}

void UndoManager::updateSavedState()
{
	m_prevDocXml = m_doc->serialize();
}

}
