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
	else if (typeStr == "integer")
	{
		QDomElement rangeElem = fromXml.firstChildElement("range");
		m_type = Integer;
		m_rangeFrom = rangeElem.attribute("from").toInt();
		m_rangeTo = rangeElem.attribute("to").toInt();
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
	m_rangeFrom = other.m_rangeFrom;
	m_rangeTo = other.m_rangeTo;
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
		case Integer:
		{
			target.setAttribute("type", "integer");
			QDomElement rangeElem = doc.createElement("range");
			target.appendChild(rangeElem);
			rangeElem.setAttribute("from", m_rangeFrom);
			rangeElem.setAttribute("to", m_rangeTo);
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

DatatypeName DatatypeName::makeInteger(int rangeFrom, int rangeTo)
{
	DatatypeName res;
	res.m_type = Integer;
	res.m_rangeFrom = rangeFrom;
	res.m_rangeTo = rangeTo;
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
		case Integer:
			return QString("integer range %1 to %2").arg(m_rangeFrom).arg(m_rangeTo);
		case Other:
			return m_datatypeName;
		case Set:
			return QString("set of %1").arg(m_innerDatatype->toString());
		default:
			qFatal("This should never happen");
			return QString();
	}
}

int DatatypeName::integerRangeFrom() const
{
	return m_rangeFrom;
}

int DatatypeName::integerRangeTo() const
{
	return m_rangeTo;
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
