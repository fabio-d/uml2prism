#ifndef CORE_COMPILER_COMPILER_H
#define CORE_COMPILER_COMPILER_H

#include "Core/Compiler/SemanticTree.h"

namespace Core
{
namespace Compiler
{
class SemanticContext;

class Compiler
{
	public:
		Compiler(const SemanticContext *context);

		void compileVariableDeclaration(const QString &name,
			const SemanticTree::Type *type,
			const SemanticTree::Expr *initialValue, bool isPersistent);

	private:
		const SemanticContext *m_context;
};

}
}

#endif // CORE_COMPILER_COMPILER_H
