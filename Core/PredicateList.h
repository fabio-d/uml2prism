#ifndef CORE_PREDICATELIST_H
#define CORE_PREDICATELIST_H

#include <QObject>
#include <QPair>

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
		Predicate(const QString &name, const QString &expression);

		const QString &name() const;
		const QString &expression() const;

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

		QStringList listNames() const;

	private:
		Document *m_doc;
		PredicateType m_contentType;
		QList<Predicate> m_predicates;
};

}

#endif // CORE_PREDICATELIST_H
