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

#ifndef CORE_GUIPROXY_H
#define CORE_GUIPROXY_H

class QDomDocument;
class QDomElement;

namespace Core
{

class UMLElement;

class GuiProxy
{
	public:
		virtual void notifyElementAdded(UMLElement *element) = 0;
		virtual void notifyElementChanged(UMLElement *element) = 0;
		virtual void notifyElementRemoved(UMLElement *element) = 0;

		virtual void storeGuiDataToXml(UMLElement *element, QDomElement &target, QDomDocument &doc) const = 0;
		virtual bool loadGuiDataFromXml(UMLElement *element, const QDomElement &source) = 0;
};

}

#endif // CORE_GUIPROXY_H
