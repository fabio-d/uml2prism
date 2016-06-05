#include "Core/PredicateList.h"

#include <QStringList>

namespace Core
{

PredicateList::PredicateList(Document *doc, Type contentType)
: m_doc(doc), m_contentType(contentType)
{
}

Document *PredicateList::document() const
{
	return m_doc;
}

PredicateList::Type PredicateList::contentType() const
{
	return m_contentType;
}

void PredicateList::clear()
{
	m_predicates.clear();
}

void PredicateList::append(const QString &name, const QString &expression)
{
	m_predicates.append(QPair<QString, QString>(name, expression));
}

QStringList PredicateList::listNames() const
{
	QStringList res;

	for (int i = 0; i < m_predicates.count(); i++)
		res.append(m_predicates[i].first);

	return res;
}

}
