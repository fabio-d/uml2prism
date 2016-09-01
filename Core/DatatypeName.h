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

#ifndef CORE_DATATYPENAME_H
#define CORE_DATATYPENAME_H

#include <QScopedPointer>
#include <QString>

class QDomDocument;
class QDomElement;

namespace Core
{

class DatatypeName
{
	public:
		enum Type
		{
			Invalid,
			Bool,
			Other,
			Set
		};

		DatatypeName();
		explicit DatatypeName(const QDomElement &fromXml);
		DatatypeName(const DatatypeName &other);
		DatatypeName &operator=(const DatatypeName &other);

		static DatatypeName makeBool();
		static DatatypeName makeOther(const QString &datatypeName);
		static DatatypeName makeSet(const DatatypeName &innerDatatype);

		Type type() const;
		QString toString() const;

		QString datatypeName() const;

		const DatatypeName *innerDatatype() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const;

	private:
		Type m_type;
		QString m_datatypeName;
		QScopedPointer<DatatypeName> m_innerDatatype;
};

}

#endif // CORE_DATATYPENAME_H
