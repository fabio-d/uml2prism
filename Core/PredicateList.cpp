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

#include "Core/PredicateList.h"

#include <QDomDocument>
#include <QStringList>

namespace Core
{

Predicate::Predicate(const QString &name, const QString &expression)
: m_name(name), m_expression(expression)
{
}

const QString &Predicate::name() const
{
	return m_name;
}

void Predicate::setName(const QString &name)
{
	m_name = name;
}

const QString &Predicate::expression() const
{
	return m_expression;
}

void Predicate::setExpression(const QString &expression)
{
	m_expression = expression;
}

PredicateList::PredicateList(Document *doc, PredicateType contentType)
: m_doc(doc), m_contentType(contentType)
{
}

Document *PredicateList::document() const
{
	return m_doc;
}

PredicateType PredicateList::contentType() const
{
	return m_contentType;
}

void PredicateList::clear()
{
	m_predicates.clear();
}

void PredicateList::append(const Predicate &p)
{
	m_predicates.append(p);
}

QStringList PredicateList::names() const
{
	QStringList res;

	foreach (const Predicate &p, m_predicates)
		res.append(p.name());

	return res;
}

const QList<Predicate> &PredicateList::predicates() const
{
	return m_predicates;
}

void PredicateList::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	foreach (const Predicate &p, m_predicates)
	{
		QDomElement predicateElem = doc.createElement("predicate");
		target.appendChild(predicateElem);

		predicateElem.setAttribute("name", p.name());
		predicateElem.appendChild(doc.createTextNode(p.expression()));
	}
}

bool PredicateList::loadFromXml(const QDomElement &source)
{
	for (QDomElement predicateElem = source.firstChildElement();
		!predicateElem.isNull();
		predicateElem = predicateElem.nextSiblingElement())
	{
		append(Predicate(predicateElem.attribute("name"), predicateElem.text()));

	}

	return true;
}

}
