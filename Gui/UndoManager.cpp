/*
 * Copyright (C) 2016 Fabio D'Urso
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "Gui/UndoManager.h"

#include "Core/Document.h"

#include <QAction>
#include <QDebug>

static constexpr const Core::Document::SerializationOptions DiagramsOnlySerializationOptions =
	Core::Document::ActivityDiagram | Core::Document::ClassDiagram;

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
			document->deserialize(oldXml, DiagramsOnlySerializationOptions);
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

			document->deserialize(newXml, DiagramsOnlySerializationOptions);
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

	connect(document, SIGNAL(deserializationCompleted(Core::Document::SerializationOptions)), this, SLOT(updateSavedState()));
	connect(undoAction, SIGNAL(triggered()), &m_undoStack, SLOT(undo()));
	connect(redoAction, SIGNAL(triggered()), &m_undoStack, SLOT(redo()));

	m_prevDocXml = m_doc->serialize(DiagramsOnlySerializationOptions);
}

void UndoManager::push(QUndoCommand *command)
{
	m_undoStack.push(command);
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
	const QByteArray newDocXml = m_doc->serialize(DiagramsOnlySerializationOptions);

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
	m_prevDocXml = m_doc->serialize(DiagramsOnlySerializationOptions);
}

}
