#include "Core/Document.h"

#include "Core/PredicateList.h"
#include "Core/UMLDiagram.h"

#include <QDomDocument>
#include <QStringList>

namespace Core
{

Document::Document(QObject *parent)
: QObject(parent), m_deserializeInProgress(false)
{
	m_activityDiagram = new UMLDiagram(this, UMLDiagram::Activity);
	m_classDiagram = new UMLDiagram(this, UMLDiagram::Class);
	m_labels = new PredicateList(this, PredicateType::Label);
	m_properties = new PredicateList(this, PredicateType::Property);
}

Document::~Document()
{
	delete m_properties;
	delete m_labels;
	delete m_activityDiagram;
	delete m_classDiagram;
}

void Document::clear()
{
	m_activityDiagram->deleteAllElements();
	m_classDiagram->deleteAllElements();
}

QStringList Document::listAllNames() const
{
	return m_activityDiagram->listNames() + m_classDiagram->listNames()
		+ m_labels->names() + m_properties->names();
}

QStringList Document::listDatatypeNames() const
{
	Q_ASSERT(m_activityDiagram->listNames(true).isEmpty());
	return m_classDiagram->listNames(true);
}

// Generate a new global name that is not currently in use
QString Document::generateFreshName(const QString &prefix) const
{
	const QStringList &allNames = listAllNames();
	QString result;
	int seqNum = 1;

	do
		result = QString("%1%2").arg(prefix).arg(seqNum++);
	while (allNames.contains(result));

	return result;
}

QByteArray Document::serialize(SerializationOptions storeWhat) const
{
	QDomDocument doc;
	QDomElement rootElem = doc.createElement("model");
	doc.appendChild(rootElem);

	if (storeWhat.testFlag(ActivityDiagram))
	{
		QDomElement activityDiagElem = doc.createElement("activity-diagram");
		rootElem.appendChild(activityDiagElem);
		m_activityDiagram->storeToXml(activityDiagElem, doc);
	}

	if (storeWhat.testFlag(ClassDiagram))
	{
		QDomElement classDiagElem = doc.createElement("class-diagram");
		rootElem.appendChild(classDiagElem);
		m_classDiagram->storeToXml(classDiagElem, doc);
	}

	if (storeWhat.testFlag(Labels))
	{
		QDomElement labelsElem = doc.createElement("labels");
		rootElem.appendChild(labelsElem);
		m_labels->storeToXml(labelsElem, doc);
	}

	if (storeWhat.testFlag(Properties))
	{
		QDomElement propertiesElem = doc.createElement("properties");
		rootElem.appendChild(propertiesElem);
		m_properties->storeToXml(propertiesElem, doc);
	}

	return doc.toByteArray(4);
}

bool Document::deserialize(const QByteArray &data, SerializationOptions loadWhat)
{
	SerializationOptions loadedWhat = NoOptions;
	m_deserializeInProgress = true;

	clear();

	QDomDocument doc;
	if (!doc.setContent(data))
	{
		m_deserializeInProgress = false;
		emit deserializationCompleted(loadedWhat);
		return false;
	}

	QDomElement rootElem = doc.documentElement();
	QDomElement activityDiagElem = rootElem.firstChildElement("activity-diagram");
	QDomElement classDiagElem = rootElem.firstChildElement("class-diagram");
	QDomElement labelsElem = rootElem.firstChildElement("labels");
	QDomElement propertiesElem = rootElem.firstChildElement("properties");

	if (loadWhat.testFlag(ActivityDiagram))
	{
		if (m_activityDiagram->loadFromXml(activityDiagElem))
		{
			loadedWhat |= ActivityDiagram;
		}
		else
		{
			m_deserializeInProgress = false;
			emit deserializationCompleted(loadedWhat);
			return false;
		}
	}

	if (loadWhat.testFlag(ClassDiagram))
	{
		if (m_classDiagram->loadFromXml(classDiagElem))
		{
			loadedWhat |= ClassDiagram;
		}
		else
		{
			m_deserializeInProgress = false;
			emit deserializationCompleted(loadedWhat);
			return false;
		}
	}

	if (loadWhat.testFlag(Labels))
	{
		if (m_labels->loadFromXml(labelsElem))
		{
			loadedWhat |= Labels;
		}
		else
		{
			m_deserializeInProgress = false;
			emit deserializationCompleted(loadedWhat);
			return false;
		}
	}

	if (loadWhat.testFlag(Properties))
	{
		if (m_properties->loadFromXml(propertiesElem))
		{
			loadedWhat |= Properties;
		}
		else
		{
			m_deserializeInProgress = false;
			emit deserializationCompleted(loadedWhat);
			return false;
		}
	}

	m_deserializeInProgress = false;
	emit deserializationCompleted(loadedWhat);
	return true;
}

bool Document::isDeserializationInProgress() const
{
	return m_deserializeInProgress;
}

}
