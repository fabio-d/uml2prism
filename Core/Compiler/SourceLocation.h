/*
 * Copyright (C) 2016 Fabio D'Urso
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef CORE_COMPILER_SOURCELOCATION_H
#define CORE_COMPILER_SOURCELOCATION_H

#include <QString>

namespace Core
{
namespace Compiler
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

#endif // CORE_COMPILER_SOURCELOCATION_H
