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

#ifndef GUI_UMLTOOLBOXLISTWIDGET_H
#define GUI_UMLTOOLBOXLISTWIDGET_H

#include <QListWidget>

namespace Gui
{

class UMLToolboxListWidget : public QListWidget
{
	Q_OBJECT

	protected:
		explicit UMLToolboxListWidget(QWidget *parent = nullptr);

		QStringList mimeTypes() const override;
		QMimeData *mimeData(const QList<QListWidgetItem*> items) const override;

		void addTool(const QString &text, const QIcon &icon,
			const QByteArray &mimeType, const QByteArray &mimeData);

	private slots:
		void slotItemClick(QListWidgetItem *item);

	private:
		QSize sizeHint() const override;
};

class UMLActivityDiagramToolboxListWidget : public UMLToolboxListWidget
{
	public:
		explicit UMLActivityDiagramToolboxListWidget(QWidget *parent = nullptr);
};

class UMLClassDiagramToolboxListWidget : public UMLToolboxListWidget
{
	public:
		explicit UMLClassDiagramToolboxListWidget(QWidget *parent = nullptr);
};

}

#endif // GUI_UMLTOOLBOXLISTWIDGET_H
