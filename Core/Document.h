#ifndef CORE_DOCUMENT_H
#define CORE_DOCUMENT_H

#include <QObject>

namespace Core
{

class UMLDiagram;

class Document : public QObject
{
	Q_OBJECT

	public:
		explicit Document(QObject *parent = nullptr);
		~Document();

		UMLDiagram *activityDiagram() { return m_activityDiagram; }
		const UMLDiagram *activityDiagram() const { return m_activityDiagram; }
		UMLDiagram *classDiagram() { return m_classDiagram; }
		const UMLDiagram *classDiagram() const { return m_classDiagram; }

		void clear();

		QByteArray serialize() const;
		bool deserialize(const QByteArray &data);
		bool isDeserializationInProgress() const;

	signals:
		void deserializationCompleted();

	private:
		UMLDiagram *m_activityDiagram;
		UMLDiagram *m_classDiagram;
		bool m_deserializeInProgress;
};

}

#endif // CORE_DOCUMENT_H
