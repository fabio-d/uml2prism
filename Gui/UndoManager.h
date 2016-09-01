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

#ifndef GUI_UNDOMANAGER_H
#define GUI_UNDOMANAGER_H

#include <QUndoStack>

class QAction;

namespace Core
{
class Document;
};

namespace Gui
{

class UndoManager : public QObject
{
	Q_OBJECT

	public:
		UndoManager(Core::Document *document, QAction *undoAction, QAction *redoAction, QObject *parent = nullptr);

		void push(QUndoCommand *command);

		void clearStack();
		void setCleanStack();
		bool isStackEmpty() const;

	signals:
		void undoCleanChanged(bool clean);

	public slots:
		void createCheckpoint();
		void updateSavedState();

	private:
		Core::Document *m_doc;
		QByteArray m_prevDocXml;

		QUndoStack m_undoStack;
};

}

#endif // GUI_UNDOMANAGER_H
