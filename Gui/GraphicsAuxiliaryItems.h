#ifndef GUI_GRAPHICSAUXILIARYITEMS_H
#define GUI_GRAPHICSAUXILIARYITEMS_H

#include "Gui/UMLGraphicsScene.h"

#include <QGraphicsSimpleTextItem>

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

		GraphicsLabelItem(QGraphicsItem *parent, Options options);

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

}

#endif // GUI_GRAPHICSAUXILIARYITEMS_H
