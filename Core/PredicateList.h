#ifndef CORE_PREDICATELIST_H
#define CORE_PREDICATELIST_H

#include <QObject>
#include <QPair>

namespace Core
{

class Document;

class PredicateList : public QObject
{
	Q_OBJECT

	public:
		enum Type
		{
			Properties,
			Labels
		};

		PredicateList(Document *doc, Type contentType);
		Type contentType() const;

		Document *document() const;

		void clear();
		void append(const QString &name, const QString &expression);

		QStringList listNames() const;

	private:
		Document *m_doc;
		Type m_contentType;
		QList<QPair<QString, QString>> m_predicates;
};

}

#endif // CORE_PREDICATELIST_H
