#include "Core/Compiler/SourceLocation.h"

namespace Core
{
namespace Compiler
{

SourceLocation::SourceLocation()
: m_line(0), m_column(0), m_endLine(0), m_endColumn(0)
{
}

SourceLocation::SourceLocation(int line, int column)
: m_line(line), m_column(column), m_endLine(0), m_endColumn(0)
{
}

SourceLocation::SourceLocation(int line, int column, int endLine, int endColumn)
: m_line(line), m_column(column), m_endLine(endLine), m_endColumn(endColumn)
{
}

int SourceLocation::line() const
{
	return m_line;
}

int SourceLocation::column() const
{
	return m_column;
}

bool SourceLocation::hasEnd() const
{
	if (m_line < m_endLine)
		return true;
	else if (m_line == m_endLine && m_column < m_endColumn)
		return true;
	else
		return false;
}

int SourceLocation::endLine() const
{
	Q_ASSERT(hasEnd() == true);
	return m_endLine;
}

int SourceLocation::endColumn() const
{
	Q_ASSERT(hasEnd() == true);
	return m_endColumn;
}

QString SourceLocation::toString() const
{
	QString res = QString("%1.%2").arg(m_line).arg(m_column);

	if (hasEnd())
	{
		if (m_endLine == m_line)
			res += QString("-%1").arg(m_endColumn);
		else
			res += QString("-%1.%2").arg(m_endLine).arg(m_endColumn);
	}

	return res;
}

}
}
