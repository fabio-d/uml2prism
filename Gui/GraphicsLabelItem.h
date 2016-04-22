#ifndef GUI_GRAPHICSLABELITEM_H
#define GUI_GRAPHICSLABELITEM_H

#include <QGraphicsSimpleTextItem>

namespace Gui
{

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

		explicit GraphicsLabelItem(QGraphicsItem *parent, Options options);

		void setText(const QString &text);
};

Q_DECLARE_OPERATORS_FOR_FLAGS(GraphicsLabelItem::Options)

}

#endif // GUI_GRAPHICSLABELITEM_H
