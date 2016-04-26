#include "Core/Document.h"

#include "Core/UMLDiagram.h"

#include <QDomDocument>

namespace Core
{

Document::Document(QObject *parent)
: QObject(parent), m_deserializeInProgress(false)
{
	m_activityDiagram = new UMLDiagram(this, UMLDiagram::Activity);
	m_classDiagram = new UMLDiagram(this, UMLDiagram::Class);
}

Document::~Document()
{
	delete m_activityDiagram;
	delete m_classDiagram;
}

void Document::clear()
{
	m_activityDiagram->deleteAllElements();
	m_classDiagram->deleteAllElements();
}

QByteArray Document::serialize() const
{
	QDomDocument doc;
	QDomElement rootElem = doc.createElement("model");
	doc.appendChild(rootElem);

	QDomElement activityDiagElem = doc.createElement("activity-diagram");
	rootElem.appendChild(activityDiagElem);
	m_activityDiagram->storeToXml(activityDiagElem, doc);

	QDomElement classDiagElem = doc.createElement("class-diagram");
	rootElem.appendChild(classDiagElem);
	m_classDiagram->storeToXml(classDiagElem, doc);

	return doc.toByteArray(4);
}

bool Document::deserialize(const QByteArray &data)
{
	m_deserializeInProgress = true;

	clear();

	QDomDocument doc;
	if (!doc.setContent(data))
	{
		m_deserializeInProgress = false;
		emit deserializationCompleted();
		return false;
	}

	QDomElement rootElem = doc.documentElement();
	QDomElement activityDiagElem = rootElem.firstChildElement("activity-diagram");
	QDomElement classDiagElem = rootElem.firstChildElement("class-diagram");

	if (!m_activityDiagram->loadFromXml(activityDiagElem))
	{
		m_deserializeInProgress = false;
		emit deserializationCompleted();
		return false;
	}

	if (!m_classDiagram->loadFromXml(classDiagElem))
	{
		m_deserializeInProgress = false;
		emit deserializationCompleted();
		return false;
	}

	m_deserializeInProgress = false;
	emit deserializationCompleted();
	return true;
}

bool Document::isDeserializationInProgress() const
{
	return m_deserializeInProgress;
}

}
