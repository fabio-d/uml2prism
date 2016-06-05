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
