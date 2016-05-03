#include "Core/ScriptLanguage/SyntaxTree.h"

namespace Core
{
namespace ScriptLanguage
{
namespace SyntaxTree
{

Expression::~Expression()
{
}

Constant::Constant(int value)
: m_value(value)
{
}

QString Constant::toString() const
{
	return QString("Constant(%1)").arg(m_value);
}

AddOp::AddOp(Expression *op1, Expression *op2)
: m_op1(op1), m_op2(op2)
{
}

AddOp::~AddOp()
{
	delete m_op1;
	delete m_op2;
}

QString AddOp::toString() const
{
	return QString("AddOp(%1, %2)").arg(m_op1->toString()).arg(m_op2->toString());
}

}
}
}
