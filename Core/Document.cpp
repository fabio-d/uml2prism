#include "Core/Document.h"

#include "Core/UMLDiagram.h"

#include <QDomDocument>

namespace Core
{

Document::Document()
{
	m_activityDiagram = new UMLDiagram(UMLDiagram::Activity);
	m_classDiagram = new UMLDiagram(UMLDiagram::Class);
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
	clear();

	QDomDocument doc;
	if (!doc.setContent(data))
		return false;

	QDomElement rootElem = doc.documentElement();
	QDomElement activityDiagElem = rootElem.firstChildElement("activity-diagram");
	QDomElement classDiagElem = rootElem.firstChildElement("class-diagram");

	if (!m_activityDiagram->loadFromXml(activityDiagElem))
		return false;

	if (!m_classDiagram->loadFromXml(classDiagElem))
		return false;

	return true;
}

}
