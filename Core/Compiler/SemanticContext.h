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

#ifndef CORE_COMPILER_SEMANTICCONTEXT_H
#define CORE_COMPILER_SEMANTICCONTEXT_H

#include "Core/Compiler/SemanticTree.h"

#include <QMap>

namespace Core
{
namespace Compiler
{

class SemanticContext
{
	friend class Compiler;

	public:
		~SemanticContext();

		const SemanticTree::BoolType *boolType() const;
		const SemanticTree::Type *findOtherType(const QString &name) const;
		const SemanticTree::SetType *findSetType(const SemanticTree::Type *innerType) const;

		SemanticTree::EnumerationType *registerEnumeration(const QString &enumName);
		SemanticTree::ClassType *registerClass(const QString &className);
		void registerGlobalVariable(const QString &name, const SemanticTree::Type *type);
		void registerSignal(const QString &name, const SemanticTree::Type *type); // type==nullptr means no attached message
		void registerState(const QString &name);
		void registerLabel(const QString &name);

		const SemanticTree::EnumerationType *findEnumerationValue(const QString &value) const;
		const SemanticTree::Type *findGlobalVariableOrSignalWithMessage(const QString &name) const;
		bool findStateOrLabelOrSignalWithoutMessage(const QString &name) const;

		bool isSignalWithMessage(const QString &name) const;
		bool isSignalWithoutMessage(const QString &name) const;
		bool isGlobalVariable(const QString &name) const;
		bool isState(const QString &name) const;
		bool isLabel(const QString &name) const;

	private:
		SemanticTree::BoolType m_boolType;
		QMap<QString, const SemanticTree::Type*> m_classAndEnumTypes; // EnumerationType and ClassType instances by name
		mutable QMap<const SemanticTree::Type*, const SemanticTree::SetType*> m_setTypes; // SetType instances by innerType
		QMap<QString, const SemanticTree::Type*> m_globalVariables;
		QMap<QString, const SemanticTree::Type*> m_signals;
		QStringList m_states, m_labels;
};

}
}

#endif // CORE_COMPILER_SEMANTICCONTEXT_H
