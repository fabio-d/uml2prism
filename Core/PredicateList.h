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
