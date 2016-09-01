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

#ifndef CORE_UMLDIAGRAM_H
#define CORE_UMLDIAGRAM_H

#include <QObject>
#include <QList>

class QDomDocument;
class QDomElement;

namespace Core
{

class Document;
class GuiProxy;
class UMLElement;

class UMLDiagram : public QObject
{
	Q_OBJECT

	public:
		enum Type { Activity, Class };

		UMLDiagram(Document *doc, Type type);
		~UMLDiagram();

		Document *document() const;
		Type type() const;

		// The GuiProxy object receives notifications about events
		void setGuiProxy(GuiProxy *guiProxy);
		GuiProxy *guiProxy() const;

		// List all elements
		const QList<UMLElement*> &elements() const;

		// Add/delete elements (delete methods also destroy the object)
		void addUMLElement(UMLElement *element);
		void deleteUMLElement(UMLElement *element);
		void deleteAllElements();

		// List all node, signal, variable and datatype names in use.
		// If datatypesOnly is set, only datatype names are returned
		QStringList listNames(bool datatypesOnly = false) const;

		// Store/load from XML element
		void storeToXml(QDomElement &target, QDomDocument &doc) const;
		bool loadFromXml(const QDomElement &source);

	private slots:
		void slotElementChanged();

	private:
		Document *m_doc;
		Type m_type;
		GuiProxy *m_guiProxy;

		// sorted by type
		QList<UMLElement*> m_elements;
};

}

#endif // CORE_UMLDIAGRAM_H
