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
