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

#include "Core/DatatypeName.h"

#include <QDomDocument>

namespace Core
{

DatatypeName::DatatypeName()
: m_type(Invalid)
{
}

DatatypeName::DatatypeName(const QDomElement &fromXml)
{
	const QString typeStr = fromXml.attribute("type");

	if (typeStr == "bool")
	{
		m_type = Bool;
	}
	else if (typeStr == "other")
	{
		m_type = Other;
		m_datatypeName = fromXml.text();
	}
	else if (typeStr == "set")
	{
		m_type = Set;
		m_innerDatatype.reset(new DatatypeName(fromXml.firstChildElement("inner-datatype")));
	}
	else
	{
		qFatal("This should never happen");
	}
}

DatatypeName::DatatypeName(const DatatypeName &other)
{
	operator=(other);
}

DatatypeName &DatatypeName::operator=(const DatatypeName &other)
{
	m_type = other.m_type;
	m_datatypeName = other.m_datatypeName;

	if (!other.m_innerDatatype.isNull())
		m_innerDatatype.reset(new DatatypeName(*other.m_innerDatatype));

	return *this;
}

void DatatypeName::storeToXml(QDomElement &target, QDomDocument &doc) const
{
	switch (m_type)
	{
		case Bool:
		{
			target.setAttribute("type", "bool");
			break;
		}
		case Other:
		{
			target.setAttribute("type", "other");
			target.appendChild(doc.createTextNode(m_datatypeName));
			break;
		}
		case Set:
		{
			target.setAttribute("type", "set");
			QDomElement innerElem = doc.createElement("inner-datatype");
			target.appendChild(innerElem);
			m_innerDatatype->storeToXml(innerElem, doc);
			break;
		}
		default:
		{
			qFatal("This should never happen");
		}
	}
}

DatatypeName DatatypeName::makeBool()
{
	DatatypeName res;
	res.m_type = Bool;
	return res;
}

DatatypeName DatatypeName::makeOther(const QString &datatypeName)
{
	DatatypeName res;
	res.m_type = Other;
	res.m_datatypeName = datatypeName;
	return res;
}

DatatypeName DatatypeName::makeSet(const DatatypeName &innerDatatype)
{
	DatatypeName res;
	res.m_type = Set;
	res.m_innerDatatype.reset(new DatatypeName(innerDatatype));
	return res;
}

DatatypeName::Type DatatypeName::type() const
{
	return m_type;
}

QString DatatypeName::toString() const
{
	switch (m_type)
	{
		case Bool:
			return "bool";
		case Other:
			return m_datatypeName;
		case Set:
			return QString("set of %1").arg(m_innerDatatype->toString());
		default:
			qFatal("This should never happen");
			return QString();
	}
}

QString DatatypeName::datatypeName() const
{
	return m_datatypeName;
}

const DatatypeName *DatatypeName::innerDatatype() const
{
	return m_innerDatatype.data();
}

}
