#include "Core/PredicateList.h"

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

const QString &Predicate::expression() const
{
	return m_expression;
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

QStringList PredicateList::listNames() const
{
	QStringList res;

	foreach (const Predicate &p, m_predicates)
		res.append(p.name());

	return res;
}

}
