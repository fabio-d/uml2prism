#ifndef CORE_DOCUMENT_H
#define CORE_DOCUMENT_H

#include <QObject>

namespace Core
{

class PredicateList;
class UMLDiagram;

class Document : public QObject
{
	Q_OBJECT

	public:
		explicit Document(QObject *parent = nullptr);
		~Document();

		enum SerializationOption
		{
			NoOptions = 0,
			ActivityDiagram = 1 << 1,
			ClassDiagram = 1 << 2,
			Labels = 1 << 3,
			Properties = 1 << 4
		};
		Q_DECLARE_FLAGS(SerializationOptions, SerializationOption)
		Q_FLAGS(SerializationOptions SerializationOption)

		UMLDiagram *activityDiagram() { return m_activityDiagram; }
		const UMLDiagram *activityDiagram() const { return m_activityDiagram; }
		UMLDiagram *classDiagram() { return m_classDiagram; }
		const UMLDiagram *classDiagram() const { return m_classDiagram; }
		PredicateList *labels() { return m_labels; }
		const PredicateList *labels() const { return m_labels; }
		PredicateList *properties() { return m_properties; }
		const PredicateList *properties() const { return m_properties; }

		// Remove all elements from activity and class diagrams
		void clear();

		// List all node, signal, variable, datatype, label and property
		// names in use
		QStringList listAllNames() const;

		// List all defined datatype names
		QStringList listDatatypeNames() const;

		// Generate a new global name that is not currently in use
		QString generateFreshName(const QString &prefix) const;

		// Save/restore document to/from XML string
		QByteArray serialize(SerializationOptions storeWhat) const;
		bool deserialize(const QByteArray &data, SerializationOptions loadWhat);
		bool isDeserializationInProgress() const;

	signals:
		void deserializationCompleted(Core::Document::SerializationOptions loadedWhat);

	private:
		UMLDiagram *m_activityDiagram;
		UMLDiagram *m_classDiagram;
		PredicateList *m_labels;
		PredicateList *m_properties;
		bool m_deserializeInProgress;
};

Q_DECLARE_OPERATORS_FOR_FLAGS(Document::SerializationOptions)

}

#endif // CORE_DOCUMENT_H
