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

class GraphicsEdgeItem : public QGraphicsLineItem
{
	public:
		GraphicsEdgeItem(QGraphicsItem *parent = nullptr);

		QRectF boundingRect() const override;
		QPainterPath shape() const override;

		QGraphicsItem *createPlaceholder(qreal atPercentage);

		void setLine(const QLineF &line);
		void setLine(qreal x1, qreal y1, qreal x2, qreal y2);

	private:
		QMultiMap<qreal, QGraphicsItem*> m_placeholders;
};

}

#endif // GUI_GRAPHICSAUXILIARYITEMS_H
