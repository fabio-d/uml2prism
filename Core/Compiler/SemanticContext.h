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
	public:
		~SemanticContext();

		const SemanticTree::BoolType *boolType() const;
		const SemanticTree::Type *findOtherType(const QString &name) const;
		const SemanticTree::SetType *findSetType(const SemanticTree::Type *innerType) const;

		SemanticTree::EnumerationType *registerEnumeration(const QString &enumName);
		SemanticTree::ClassType *registerClass(const QString &className);
		void registerGlobalVariable(const QString &name, const SemanticTree::Type *type, const SemanticTree::Expr *initVal);
		void registerSignal(const QString &name, const SemanticTree::Type *type); // type==nullptr means no attached message
		void registerState(const QString &name);

		const SemanticTree::EnumerationType *findEnumerationValue(const QString &value) const;
		const SemanticTree::Type *findGlobalVariableOrSignalWithMessage(const QString &name) const;
		bool findStateOrSignalWithoutMessage(const QString &name) const;

	private:
		SemanticTree::BoolType m_boolType;
		QMap<QString, const SemanticTree::Type*> m_classAndEnumTypes; // EnumerationType and ClassType instances by name
		mutable QMap<const SemanticTree::Type*, const SemanticTree::SetType*> m_setTypes; // SetType instances by innerType
		QMap<QString, const SemanticTree::Type*> m_globalVariables;
		QMap<QString, const SemanticTree::Type*> m_signals;
		QStringList m_states;
};

}
}

#endif // CORE_COMPILER_SEMANTICCONTEXT_H
