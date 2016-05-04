#ifndef CORE_SCRIPTLANGUAGE_SOURCELOCATION_H
#define CORE_SCRIPTLANGUAGE_SOURCELOCATION_H

#include <QString>

namespace Core
{
namespace ScriptLanguage
{

class SourceLocation
{
	public:
		SourceLocation();
		SourceLocation(int line, int column);
		SourceLocation(int line, int column, int endLine, int endColumn);

		int line() const;
		int column() const;

		bool hasEnd() const;
		int endLine() const;
		int endColumn() const;

		QString toString() const;

	private:
		int m_line, m_column, m_endLine, m_endColumn;
};

}
}

#endif // CORE_SCRIPTLANGUAGE_SOURCELOCATION_H
