#ifndef CORE_DATATYPENAME_H
#define CORE_DATATYPENAME_H

#include <QScopedPointer>
#include <QString>

class QDomDocument;
class QDomElement;

namespace Core
{

class DatatypeName
{
	public:
		enum Type
		{
			Invalid,
			Bool,
			Integer,
			Other,
			Set
		};

		DatatypeName();
		explicit DatatypeName(const QDomElement &fromXml);
		DatatypeName(const DatatypeName &other);
		DatatypeName &operator=(const DatatypeName &other);

		static DatatypeName makeBool();
		static DatatypeName makeInteger(int rangeFrom, int rangeTo);
		static DatatypeName makeOther(const QString &datatypeName);
		static DatatypeName makeSet(const DatatypeName &innerDatatype);

		Type type() const;
		QString toString() const;

		int integerRangeFrom() const;
		int integerRangeTo() const;

		QString datatypeName() const;

		const DatatypeName *innerDatatype() const;

		void storeToXml(QDomElement &target, QDomDocument &doc) const;

	private:
		Type m_type;
		int m_rangeFrom, m_rangeTo;
		QString m_datatypeName;
		QScopedPointer<DatatypeName> m_innerDatatype;
};

}

#endif // CORE_DATATYPENAME_H
