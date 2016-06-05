#ifndef CORE_PROPERTYLIST_H
#define CORE_PROPERTYLIST_H

#include <QObject>
#include <QPair>

namespace Core
{

class Document;

class PropertyList : public QObject
{
	Q_OBJECT

	public:
		enum Type
		{
			Properties,
			Labels
		};

		PropertyList(Document *doc, Type contentType);
		Type contentType() const;

		Document *document() const;

		void clear();
		void append(const QString &name, const QString &expression);

		QStringList listNames() const;

	private:
		Document *m_doc;
		Type m_contentType;
		QList<QPair<QString, QString>> m_properties;
};

}

#endif // CORE_PROPERTYLIST_H
