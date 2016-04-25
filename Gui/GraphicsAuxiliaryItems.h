#ifndef GUI_GRAPHICSAUXILIARYITEMS_H
#define GUI_GRAPHICSAUXILIARYITEMS_H

#include "Gui/UMLGraphicsScene.h"

#include <QGraphicsSimpleTextItem>
#include <QMultiMap>

namespace Gui
{

class UMLElement;

class GraphicsLabelItem : public QGraphicsSimpleTextItem
{
	public:
		enum Option
		{
			NoOptions = 0,
			InitiallyOnTheRight = 1 << 1,
			InitiallyOnTheBottom = 1 << 2,
			NonMovable = 1 << 3
		};
		Q_DECLARE_FLAGS(Options, Option)

		explicit GraphicsLabelItem(Options options, QGraphicsItem *parent = nullptr);

		void setText(const QString &text);
		Options options() const;

	private:
		Options m_options;
};

Q_DECLARE_OPERATORS_FOR_FLAGS(GraphicsLabelItem::Options)

// QGraphicsItem decorator class that itercepts ItemPositionHasChanged events
// and reports them to the UMLGraphicsScene object
template <class GraphicsItemType>
class GraphicsPositionChangeSpyItem : public GraphicsItemType
{
	public:
		template <typename ...Args>
		explicit GraphicsPositionChangeSpyItem(UMLElement *watchedElement, Args&&... args) 
		: GraphicsItemType(args...), watchedElement(watchedElement)
		{
			GraphicsItemType::setFlag(QGraphicsItem::ItemSendsGeometryChanges, true);
		}

	protected:
		QVariant itemChange(QGraphicsItem::GraphicsItemChange change, const QVariant &value) override
		{
			if (change == QGraphicsItem::ItemPositionHasChanged)
			{
				QGraphicsScene *sc = GraphicsItemType::scene();
				if (sc != nullptr)
					static_cast<UMLGraphicsScene*>(sc)->notifyGeometryChanged(watchedElement);
			}

			return GraphicsItemType::itemChange(change, value);
		}

	private:
		UMLElement *watchedElement;
};

class GraphicsEdgeItem : public QGraphicsPathItem
{
	public:
		class MoveWatcher
		{
			public:
				virtual void notifyEdgeMoved(const QPointF &delta) = 0;
		};

		GraphicsEdgeItem(QGraphicsItem *parent = nullptr);

		QRectF boundingRect() const override;
		QPainterPath shape() const override;

		QGraphicsItem *createPlaceholder(qreal atPercent);
		void setWatcher(MoveWatcher *watcher);

		void setPolyline(const QPolygonF &polyline);

	protected:
		QVariant itemChange(GraphicsItemChange change, const QVariant &value) override;

	private:
		QMultiMap<qreal, QGraphicsItem*> m_placeholders;
		MoveWatcher *m_watcher;
		QPointF m_lastKnownPos;
};

class GraphicsDatatypeItem : public QGraphicsRectItem
{
	public:
		explicit GraphicsDatatypeItem(bool showEnumerationStereotype, QGraphicsItem *parent = nullptr);

		void setName(const QString &text);
		void setContents(const QString &text);

		void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
			QWidget *widget = nullptr) override;

	private:
		void relayout();

		QGraphicsSimpleTextItem *m_stereotype;
		QGraphicsSimpleTextItem *m_name;
		QGraphicsSimpleTextItem *m_contents;
};

}

#endif // GUI_GRAPHICSAUXILIARYITEMS_H
