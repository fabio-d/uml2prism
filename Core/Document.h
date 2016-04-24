#ifndef CORE_DOCUMENT_H
#define CORE_DOCUMENT_H

#include <QString>

namespace Core
{

class UMLDiagram;

class Document
{
	public:
		Document();
		~Document();

		UMLDiagram *activityDiagram() { return m_activityDiagram; }
		const UMLDiagram *activityDiagram() const { return m_activityDiagram; }
		UMLDiagram *classDiagram() { return m_classDiagram; }
		const UMLDiagram *classDiagram() const { return m_classDiagram; }

		void clear();

		QByteArray serialize() const;
		bool deserialize(const QByteArray &data);

	private:
		UMLDiagram *m_activityDiagram;
		UMLDiagram *m_classDiagram;
};

}

#endif // CORE_DOCUMENT_H
