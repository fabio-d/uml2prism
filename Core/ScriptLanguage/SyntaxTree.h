#ifndef CORE_SCRIPTLANGUAGE_SYNTAXTREE_H
#define CORE_SCRIPTLANGUAGE_SYNTAXTREE_H

#include <QString>

namespace Core
{
namespace ScriptLanguage
{
namespace SyntaxTree
{

class Expression
{
	public:
		virtual ~Expression();

		virtual QString toString() const = 0;
};

class Constant : public Expression
{
	public:
		explicit Constant(int value);

		QString toString() const override;

	private:
		int m_value;
};

class AddOp : public Expression
{
	public:
		AddOp(Expression *op1, Expression *op2);
		~AddOp() override;

		QString toString() const override;

	private:
		Expression *m_op1, *m_op2;
};

}
}
}

#endif // CORE_SCRIPTLANGUAGE_SYNTAXTREE_H
