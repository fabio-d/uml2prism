#ifndef GUI_GRAPHICSLABELITEM_H
#define GUI_GRAPHICSLABELITEM_H

#include <QGraphicsSimpleTextItem>

namespace Gui
{

class GraphicsLabelItem : public QGraphicsSimpleTextItem
{
	public:
		explicit GraphicsLabelItem(QGraphicsItem *parent, bool onTheRightByDefault = false);

		void setText(const QString &text);
};

}

#endif // GUI_GRAPHICSLABELITEM_H
