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

#ifndef GUI_UMLGRAPHICSCENE_H
#define GUI_UMLGRAPHICSCENE_H

#include "Core/GuiProxy.h"

#include <QGraphicsScene>
#include <QMap>
#include <QList>

class QAction;
class QMenu;
class QToolBar;

namespace Core
{
class UMLDiagram;
}

namespace Gui
{
class UMLElement;
class UMLNodeElement;

class UMLGraphicsScene : public QGraphicsScene, private Core::GuiProxy
{
	Q_OBJECT

	public:
		explicit UMLGraphicsScene(Core::UMLDiagram *dia, QObject *parent = nullptr);
		~UMLGraphicsScene();

		static const QFont &sceneFont();
		static const QFont &sceneCodeFont();

		void notifyGeometryChanged(UMLElement *element);

		void renameSelectedItem(QWidget *requestingWidget);
		void editSelectedItem(QWidget *requestingWidget);
		void deleteSelectedItems(QWidget *requestingWidget);
		void resetLabelPosition();

	signals:
		void sceneRectMayHaveChanged();
		void edgeConstructionStateChanged(bool inProgress);
		void actionsEnabledChanged(bool editEnabled, bool renameEnabled, bool deleteEnabled, bool resetLabelPosEnabled);
		void fillContextMenu(QMenu *menu);
		void undoCheckpointCreationRequest();

	private slots:
		void slotSelectionChanged();
		void createUndoCheckpoint();

	private:
		UMLNodeElement *searchNodeElementAt(const QPointF &scenePos) const;
		void scheduleUndoCheckpoint();

		void contextMenuEvent(QGraphicsSceneContextMenuEvent *contextMenuEvent) override;
		void dragMoveEvent(QGraphicsSceneDragDropEvent *event) override;
		void drawForeground(QPainter *painter, const QRectF &rect) override;
		void dropEvent(QGraphicsSceneDragDropEvent *event) override;
		void keyPressEvent(QKeyEvent *keyEvent) override;
		void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *mouseEvent) override;
		void mousePressEvent(QGraphicsSceneMouseEvent *mouseEvent) override;
		void mouseMoveEvent(QGraphicsSceneMouseEvent *mouseEvent) override;
		void mouseReleaseEvent(QGraphicsSceneMouseEvent *mouseEvent) override;

		void notifyElementAdded(Core::UMLElement *element) override;
		void notifyElementChanged(Core::UMLElement *element) override;
		void notifyElementRemoved(Core::UMLElement *element) override;
		void storeGuiDataToXml(Core::UMLElement *element, QDomElement &target, QDomDocument &doc) const override;
		bool loadGuiDataFromXml(Core::UMLElement *element, const QDomElement &source) override;

		static bool canBeEdited(Core::UMLElement *element);
		static bool canBeRenamed(Core::UMLElement *element);
		static bool canResetLabelPos(Core::UMLElement *element);

		// The UML diagram
		Core::UMLDiagram *m_dia;

		// Currently selected items. m_strictSelection is a subset of
		// m_relaxedSelection that only contains those items whose
		// main UMLElement is selected.
		QList<UMLElement*> m_strictSelection, m_relaxedSelection;

		// Information used to create graph edges
		UMLNodeElement *m_edgeConstructionOrigin;
		QPolygonF m_edgeConstructionPoints;
		bool m_edgeConstructSignal; // false=control flow, true=signal
		QPointF m_mousePos;

		// Was something moved since the last undo checkpoint?
		bool m_somethingWasMoved;

		// The creation of undo checkpoints is deferred at the end of
		// the event queue, so that multiple related events do not
		// result in multiple calls. This variable keeps track of
		// whether the creation of a checkpoint is already pending or
		// not
		bool m_undoIsAlreadyScheduled;
};

}

#endif // GUI_UMLGRAPHICSCENE_H
