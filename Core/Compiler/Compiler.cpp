#include "Core/Compiler/Compiler.h"

#include "Core/Compiler/SemanticContext.h"

#include <QDebug>

namespace Core
{
namespace Compiler
{

Compiler::Compiler(const SemanticContext *context)
: m_context(context)
{
}

void Compiler::compileVariableDeclaration(const QString &name,
	const SemanticTree::Type *type, const SemanticTree::Expr *initialValue,
	bool isPersistent)
{
	const QByteArray n = name.toLatin1();
	char *rawResult = (char*)hsCompileVariableDeclaration(
		(void*)n.constData(),
		initialValue->haskellHandle());
	const QString result = QString("// %1 %2 : %3\n%4")
		.arg(isPersistent ? "Variable" : "Persistent variable")
		.arg(name)
		.arg(type->datatypeName())
		.arg(rawResult);
	free(rawResult);

	qDebug() << result.toLatin1().constData();
}

}
}
