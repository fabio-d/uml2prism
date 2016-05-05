#ifndef CORE_MODELBUILDER_H
#define CORE_MODELBUILDER_H

#include <QString>

namespace Core
{

class Document;

class ModelBuilder
{
	public:
		explicit ModelBuilder(const Document *doc);
		~ModelBuilder();

	private:
		void emitError(const QString &location, const QString &description);

		void checkDuplicateNames();

		const Document *m_doc;
		bool m_error;
};

}

#endif // CORE_MODELBUILDER_H
