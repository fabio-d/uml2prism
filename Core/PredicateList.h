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

#ifndef CORE_PREDICATELIST_H
#define CORE_PREDICATELIST_H

#include <QObject>
#include <QPair>

class QDomDocument;
class QDomElement;

namespace Core
{

class Document;

enum class PredicateType
{
	Property,
	Label
};

class Predicate
{
	public:
		Predicate() = default;
		Predicate(const QString &name, const QString &expression);

		const QString &name() const;
		void setName(const QString &name);

		const QString &expression() const;
		void setExpression(const QString &name);

	private:
		QString m_name, m_expression;
};

class PredicateList : public QObject
{
	Q_OBJECT

	public:


		PredicateList(Document *doc, PredicateType contentType);
		PredicateType contentType() const;

		Document *document() const;

		void clear();
		void append(const Predicate &p);

		QStringList names() const;
		const QList<Predicate> &predicates() const;

		// Store/load from XML element
		void storeToXml(QDomElement &target, QDomDocument &doc) const;
		bool loadFromXml(const QDomElement &source);

	private:
		Document *m_doc;
		PredicateType m_contentType;
		QList<Predicate> m_predicates;
};

}

#endif // CORE_PREDICATELIST_H
