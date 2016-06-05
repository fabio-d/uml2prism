#include "Core/PropertyList.h"

#include <QStringList>

namespace Core
{

PropertyList::PropertyList(Document *doc, Type contentType)
: m_doc(doc), m_contentType(contentType)
{
}

Document *PropertyList::document() const
{
	return m_doc;
}

PropertyList::Type PropertyList::contentType() const
{
	return m_contentType;
}

void PropertyList::clear()
{
	m_properties.clear();
}

void PropertyList::append(const QString &name, const QString &expression)
{
	m_properties.append(QPair<QString, QString>(name, expression));
}

QStringList PropertyList::listNames() const
{
	QStringList res;

	for (int i = 0; i < m_properties.count(); i++)
		res.append(m_properties[i].first);

	return res;
}

}
