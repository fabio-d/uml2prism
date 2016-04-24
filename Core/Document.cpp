#include "Core/Document.h"

#include "Core/UMLDiagram.h"

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
	return "stub!";
}

bool Document::deserialize(const QByteArray &data)
{
	return data == "stub!";
}

}
