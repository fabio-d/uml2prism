#include "Core/Compiler/SemanticTreeGenerator.h"

#include "Core/Compiler/SemanticContext.h"
#include "Core/Compiler/SyntaxTreeGenerator.h"

#include <QDebug>

namespace Core
{
namespace Compiler
{

SemanticTreeGenerator::SemanticTreeGenerator(const QString &sourceCode, const SemanticTree::Type *valueType, const SemanticContext *context)
: m_success(true), m_context(context), m_resultExpr(nullptr), m_resultStmt(nullptr)
{
	SyntaxTreeGenerator sygen(sourceCode, SyntaxTreeGenerator::Value);
	if (!sygen.success())
	{
		setError(sygen.errorLocation(), sygen.errorMessage());
		return;
	}

	m_resultExpr = convertExpression(sygen.resultValue(), valueType);
}

SemanticTreeGenerator::SemanticTreeGenerator(const QString &sourceCode, const SemanticContext *context,
	const QStringList &writableSignals, const QMap<QString, QString> &labelMap)
: m_success(true), m_context(context), m_resultExpr(nullptr), m_resultStmt(nullptr)
{
	SyntaxTreeGenerator sygen(sourceCode, SyntaxTreeGenerator::Script);
	if (!sygen.success())
	{
		setError(sygen.errorLocation(), sygen.errorMessage());
		return;
	}

	m_resultStmt = convertStatement(sygen.resultScript());
}

SemanticTreeGenerator::~SemanticTreeGenerator()
{
	delete m_resultExpr;
	delete m_resultStmt;
}

bool SemanticTreeGenerator::success() const
{
	return m_success;
}

const SourceLocation &SemanticTreeGenerator::errorLocation() const
{
	Q_ASSERT(m_success == false);
	return m_errorLocation;
}

const QString &SemanticTreeGenerator::errorMessage() const
{
	Q_ASSERT(m_success == false);
	return m_errorMessage;
}

const SemanticTree::Expr *SemanticTreeGenerator::takeResultExpr()
{
	Q_ASSERT(m_success == true);
	Q_ASSERT(m_resultExpr != nullptr);

	const SemanticTree::Expr *res = m_resultExpr;
	m_resultExpr = nullptr;
	return res;
}

const SemanticTree::Stmt *SemanticTreeGenerator::takeResultStmt()
{
	Q_ASSERT(m_success == true);
	Q_ASSERT(m_resultStmt != nullptr);

	const SemanticTree::Stmt *res = m_resultStmt;
	m_resultStmt = nullptr;
	return res;
}

void SemanticTreeGenerator::setError(const SourceLocation &location, const QString &message)
{
	// setError must not be called more than once
	Q_ASSERT(m_success == true);

	m_errorLocation = location;
	m_errorMessage = message;
	m_success = false;

	qDebug() << "SemanticTreeGenerator failed at" << location.toString() << ":" << message;
}

void SemanticTreeGenerator::setUnexpectedTypeError(const SourceLocation &location, const SemanticTree::Type *expectedType, const SemanticTree::Type *actualType)
{
	setError(location,
		 QString("Expected %1 but found %2")
			.arg(expectedType->datatypeName())
			.arg(actualType->datatypeName()));
}

const SemanticTree::Identifier *SemanticTreeGenerator::resolveIdentifier(const SyntaxTree::Identifier *ident)
{
	QList<const SyntaxTree::MemberIdentifier*> memberSegments;

	const SyntaxTree::MemberIdentifier *currSegm = dynamic_cast<const SyntaxTree::MemberIdentifier*>(ident);
	const SyntaxTree::Identifier *nextSegm = ident;

	while (currSegm != nullptr)
	{
		memberSegments.insert(0, currSegm);
		nextSegm = currSegm->container();
		currSegm = dynamic_cast<const SyntaxTree::MemberIdentifier*>(nextSegm);
	}

	const SyntaxTree::GlobalIdentifier *baseVar = static_cast<const SyntaxTree::GlobalIdentifier*>(nextSegm);
	const SemanticTree::Type *varType = m_context->findGlobalVariableOrSignalWithMessage(baseVar->name());
	if (varType == nullptr && m_context->findStateOrSignalWithoutMessage(baseVar->name()))
		varType = m_context->boolType();

	if (varType == nullptr)
	{
		setError(baseVar->location(), QString("Unresolved identifier: %1").arg(baseVar->name()));
		return nullptr;
	}

	QScopedPointer<const SemanticTree::Identifier> res(new SemanticTree::GlobalIdentifier(baseVar->name(), varType));
	foreach (const SyntaxTree::MemberIdentifier *m, memberSegments)
	{
		const SemanticTree::ClassType *classType = dynamic_cast<const SemanticTree::ClassType*>(varType);
		if (classType == nullptr)
		{
			setError(baseVar->location(), QString("Not a class type: %1").arg(varType->datatypeName()));
			return nullptr;
		}

		varType = classType->findMemberVariable(m->name());
		if (varType == nullptr)
		{
			setError(baseVar->location(), QString("%1 has no member variable called %2").arg(classType->datatypeName(), m->name()));
			return nullptr;
		}

		res.reset(new SemanticTree::MemberIdentifier(res.take(), m->name(), varType));
	}

	return res.take();
}

const SemanticTree::Type *SemanticTreeGenerator::deduceType(const SyntaxTree::Expression *expression)
{
	//qDebug() << "Deducing type of expression" << expression->toString();

	switch (expression->nodeType())
	{
		case SyntaxTree::NodeType::GlobalIdentifier:
		{
			const SyntaxTree::GlobalIdentifier *node =
				static_cast<const SyntaxTree::GlobalIdentifier*>(expression);

			// Try to resolve it as variable or signal name
			const SemanticTree::EnumerationType *enumType = m_context->findEnumerationValue(node->name());
			const SemanticTree::Type *varType = m_context->findGlobalVariableOrSignalWithMessage(node->name());
			if (varType == nullptr && m_context->findStateOrSignalWithoutMessage(node->name()))
				varType = m_context->boolType();

			if (enumType != nullptr)
			{
				return enumType;
			}
			else if (varType != nullptr)
			{
				return varType;
			}
			else
			{
				setError(expression->location(), QString("Unresolved identifier: %1").arg(node->name()));
				return nullptr;
			}
		}
		case SyntaxTree::NodeType::MemberIdentifier:
		{
			const SyntaxTree::MemberIdentifier *node =
				static_cast<const SyntaxTree::MemberIdentifier*>(expression);
			QScopedPointer<const SemanticTree::Identifier> res(resolveIdentifier(node));
			if (res.isNull())
			{
				return nullptr;
			}
			else
			{
				return res->type();
			}
		}
		case SyntaxTree::NodeType::NilLiteral:
		{
			// The type of nil cannot be deduced
			return nullptr;
		}
		case SyntaxTree::NodeType::BoolLiteral:
		{
			return m_context->boolType();
		}
		case SyntaxTree::NodeType::NotOperator:
		{
			return m_context->boolType();
		}
		case SyntaxTree::NodeType::BinaryOperator:
		{
			return m_context->boolType();
		}
		case SyntaxTree::NodeType::Tuple:
		{
			// The type of a tuple cannot be deduced
			return nullptr;
		}
		case SyntaxTree::NodeType::MethodCall:
		{
			const SyntaxTree::MethodCall *node =
				static_cast<const SyntaxTree::MethodCall*>(expression);

			QScopedPointer<const SemanticTree::Identifier> setObject(expectSetMethod(node, "contains"));
			if (setObject.isNull())
				return nullptr;
			else
				return m_context->boolType();
		}
		default:
			qFatal("This should never happen");
			return nullptr;
	}
}

// Check that mCall is a valid operation on a set, and return the set identifier.
const SemanticTree::Identifier *SemanticTreeGenerator::expectSetMethod(const SyntaxTree::MethodCall *mCall, const QString &methodName)
{
	if (mCall->object() == nullptr)
	{
		setError(mCall->location(),
			QString("No function called %1").arg(mCall->methodName()));
		return nullptr;
	}

	QScopedPointer<const SemanticTree::Identifier> object(resolveIdentifier(mCall->object()));
	if (object.isNull())
		return nullptr;

	const SemanticTree::SetType *setType = dynamic_cast<const SemanticTree::SetType*>(object->type());
	if (setType == nullptr)
	{
		setError(mCall->location(), QString("Not a set type: %1").arg(object->type()->datatypeName()));
		return nullptr;
	}

	if (mCall->methodName() != methodName)
	{
		setError(mCall->location(),
			QString("Invalid method: %1").arg(mCall->methodName()));
		return nullptr;
	}

	if (mCall->arguments().count() == 0)
	{
		setError(mCall->location(), "One argument is required");
		return nullptr;
	}

	if (mCall->arguments().count() > 1)
	{
		setError(mCall->location(), "Too many arguments: method takes only one argument");
		return nullptr;
	}

	return object.take();
}

const SemanticTree::Expr *SemanticTreeGenerator::convertExpression(const SyntaxTree::Expression *expression, const SemanticTree::Type *expectedType)
{
	//qDebug() << "Converting expression" << expression->toString() << "to type" << expectedType->datatypeName();

	switch (expression->nodeType())
	{
		case SyntaxTree::NodeType::GlobalIdentifier:
		{
			const SyntaxTree::GlobalIdentifier *node =
				static_cast<const SyntaxTree::GlobalIdentifier*>(expression);

			// Try to resolve it as variable or signal name
			const SemanticTree::EnumerationType *enumType = m_context->findEnumerationValue(node->name());
			const SemanticTree::Type *varType = m_context->findGlobalVariableOrSignalWithMessage(node->name());
			if (varType == nullptr && m_context->findStateOrSignalWithoutMessage(node->name()))
				varType = m_context->boolType();

			if (enumType != nullptr)
			{
				if (expectedType == enumType)
				{
					return new SemanticTree::ExprEnumLiteral(enumType, node->name());
				}
				else
				{
					setUnexpectedTypeError(expression->location(), expectedType, enumType);
					return nullptr;
				}
			}
			else if (varType != nullptr)
			{
				if (expectedType == varType)
				{
					return new SemanticTree::ExprVariable(new SemanticTree::GlobalIdentifier(node->name(), varType));
				}
				else
				{
					setUnexpectedTypeError(expression->location(), expectedType, varType);
					return nullptr;
				}
			}
			else
			{
				setError(expression->location(), QString("Unresolved identifier: %1").arg(node->name()));
				return nullptr;
			}
		}
		case SyntaxTree::NodeType::MemberIdentifier:
		{
			const SyntaxTree::MemberIdentifier *node =
				static_cast<const SyntaxTree::MemberIdentifier*>(expression);
			const SemanticTree::Identifier *res = resolveIdentifier(node);
			if (res == nullptr)
			{
				return nullptr;
			}
			else if (expectedType == res->type())
			{
				return new SemanticTree::ExprVariable(res);
			}
			else
			{
				setUnexpectedTypeError(expression->location(), expectedType, res->type());
				return nullptr;
			}
		}
		case SyntaxTree::NodeType::NilLiteral:
		{
			const SemanticTree::EnumerationType *enumType =
				dynamic_cast<const SemanticTree::EnumerationType*>(expectedType);
			const SemanticTree::ClassType *classType =
				dynamic_cast<const SemanticTree::ClassType*>(expectedType);

			if (enumType != nullptr)
			{
				return new SemanticTree::ExprEnumLiteral(enumType);
			}
			else if (classType != nullptr)
			{
				return new SemanticTree::ExprClassNilLiteral(classType);
			}
			else
			{
				setError(expression->location(), QString("Cannot convert nil to %1").arg(expectedType->datatypeName()));
				return nullptr;
			}
		}
		case SyntaxTree::NodeType::BoolLiteral:
		{
			const SyntaxTree::BoolLiteral *node =
				static_cast<const SyntaxTree::BoolLiteral*>(expression);

			if (expectedType == m_context->boolType())
			{
				return new SemanticTree::ExprBoolLiteral(node->value());
			}
			else
			{
				setUnexpectedTypeError(expression->location(), expectedType, m_context->boolType());
				return nullptr;
			}
		}
		case SyntaxTree::NodeType::NotOperator:
		{
			const SyntaxTree::NotOperator *node =
				static_cast<const SyntaxTree::NotOperator*>(expression);
			QScopedPointer<const SemanticTree::Expr> inner(convertExpression(node->arg(), m_context->boolType()));
			if (inner.isNull())
			{
				return nullptr;
			}
			else if (expectedType == m_context->boolType())
			{
				return new SemanticTree::ExprNotOp(inner.take());
			}
			else
			{
				setUnexpectedTypeError(expression->location(), expectedType, m_context->boolType());
				return nullptr;
			}
		}
		case SyntaxTree::NodeType::BinaryOperator:
		{
			const SyntaxTree::BinaryOperator *node =
				static_cast<const SyntaxTree::BinaryOperator*>(expression);
			const SemanticTree::Type *operandType;

			if (node->op() == SyntaxTree::BinaryOperator::NotEqual ||
				node->op() == SyntaxTree::BinaryOperator::Equal)
			{
				// Attempt to deduce operand types. Note that
				// type deduction can emit errors, and we have
				// to stop immediatly should that happen.
				operandType = deduceType(node->arg1());
				if (!m_success)
					return nullptr;

				if (operandType == nullptr)
				{
					operandType = deduceType(node->arg2());
					if (!m_success)
						return nullptr;
				}

				if (operandType == nullptr)
				{
					setError(expression->location(), "Failed to deduce compared operands' type");
					return nullptr;
				}
			}
			else // Binary AND or OR
			{
				operandType = m_context->boolType();
			}

			QScopedPointer<const SemanticTree::Expr> op1(
				convertExpression(node->arg1(), operandType));
			if (op1.isNull())
					return nullptr;
			QScopedPointer<const SemanticTree::Expr> op2(
				convertExpression(node->arg2(), operandType));
			if (op2.isNull())
					return nullptr;

			if (expectedType != m_context->boolType())
			{
				setUnexpectedTypeError(expression->location(), expectedType, m_context->boolType());
				return nullptr;
			}

			SemanticTree::ExprBinOp::Operator op;
			switch (node->op())
			{
				case SyntaxTree::BinaryOperator::Equal:
					op = SemanticTree::ExprBinOp::Equal;
					break;
				case SyntaxTree::BinaryOperator::NotEqual:
					op = SemanticTree::ExprBinOp::NotEqual;
					break;
				case SyntaxTree::BinaryOperator::And:
					op = SemanticTree::ExprBinOp::And;
					break;
				case SyntaxTree::BinaryOperator::Or:
					op = SemanticTree::ExprBinOp::Or;
					break;
			}

			return new SemanticTree::ExprBinOp(op, op1.take(), op2.take());
		}
		case SyntaxTree::NodeType::Tuple:
		{
			const SyntaxTree::Tuple *node =
				static_cast<const SyntaxTree::Tuple*>(expression);
			const SemanticTree::ClassType *classType =
				dynamic_cast<const SemanticTree::ClassType*>(expectedType);
			const SemanticTree::SetType *setType =
				dynamic_cast<const SemanticTree::SetType*>(expectedType);
			const QList<SyntaxTree::Expression*> &elements = node->elements();
			if (classType != nullptr)
			{
				QStringList memberNames = classType->memberVariables();
				if (elements.count() < memberNames.count())
				{
					setError(expression->location(),
						QString("Too few elements for a %1")
							.arg(classType->datatypeName()));
					return nullptr;
				}
				else if (elements.count() > memberNames.count())
				{
					setError(expression->location(),
						QString("Too many elements for a %1")
							.arg(classType->datatypeName()));
					return nullptr;
				}
				else
				{
					QList<const SemanticTree::Expr*> values;
					for (int i = 0; i < memberNames.count(); i++)
					{
						const SemanticTree::Type *membType = classType->findMemberVariable(memberNames[i]);
						const SyntaxTree::Expression *e = elements[i];
						const SemanticTree::Expr *val = convertExpression(e, membType);
						if (val != nullptr)
						{
							values.append(val);
						}
						else
						{
							qDeleteAll(values);
							return nullptr;
						}
					}
					return new SemanticTree::ExprTuple(classType, values);
				}
			}
			else if (setType != nullptr)
			{
				QList<const SemanticTree::Expr*> values;
				foreach (const SyntaxTree::Expression *e, elements)
				{
					const SemanticTree::Expr *val = convertExpression(e, setType->innerType());
					if (val != nullptr)
					{
						values.append(val);
					}
					else
					{
						qDeleteAll(values);
						return nullptr;
					}
				}
				return new SemanticTree::ExprTuple(setType, values);
			}
			else
			{
				setError(expression->location(),
					QString("Expected %1 but found tuple")
						.arg(expectedType->datatypeName()));
				return nullptr;
			}
		}
		case SyntaxTree::NodeType::MethodCall:
		{
			const SyntaxTree::MethodCall *node =
				static_cast<const SyntaxTree::MethodCall*>(expression);

			QScopedPointer<const SemanticTree::Identifier> setObject(expectSetMethod(node, "contains"));
			if (setObject.isNull())
				return nullptr;

			const SemanticTree::SetType *setType =
				static_cast<const SemanticTree::SetType*>(setObject->type());
			const SyntaxTree::Expression *arg = node->arguments().first();
			const SemanticTree::Expr *argval = convertExpression(arg, setType->innerType());
			if (argval == nullptr)
				return nullptr;

			return new SemanticTree::ExprSetContains(setObject.take(), argval);
		}
		default:
			qFatal("This should never happen");
			return nullptr;
	}
}

const SemanticTree::Stmt *SemanticTreeGenerator::convertStatement(const SyntaxTree::Statement *statement)
{
	qDebug() << "Converting statement" << statement->toString();

	switch (statement->nodeType())
	{
		case SyntaxTree::NodeType::CompoundStatement:
		{
			const SyntaxTree::CompoundStatement *node =
				static_cast<const SyntaxTree::CompoundStatement*>(statement);
			QList<const SemanticTree::Stmt*> statements;
			foreach (const SyntaxTree::Statement *st, node->statements())
			{
				const SemanticTree::Stmt *currSt = convertStatement(st);
				if (currSt == nullptr)
				{
					qDeleteAll(statements);
					return nullptr;
				}

				statements.append(currSt);
			}

			return new SemanticTree::StmtCompound(statements);
		}
		case SyntaxTree::NodeType::MethodCall:
		{
			const SyntaxTree::MethodCall *node =
				static_cast<const SyntaxTree::MethodCall*>(statement);

			QScopedPointer<const SemanticTree::Identifier> setObject(expectSetMethod(node, "insert"));
			if (setObject.isNull())
				return nullptr;

			const SemanticTree::SetType *setType =
				static_cast<const SemanticTree::SetType*>(setObject->type());
			const SyntaxTree::Expression *arg = node->arguments().first();
			const SemanticTree::Expr *argval = convertExpression(arg, setType->innerType());
			if (argval == nullptr)
				return nullptr;

			return new SemanticTree::StmtSetInsert(setObject.take(), argval);
		}
		case SyntaxTree::NodeType::Assignment:
		{
			const SyntaxTree::Assignment *node =
				static_cast<const SyntaxTree::Assignment*>(statement);
			QScopedPointer<const SemanticTree::Identifier> dest(resolveIdentifier(node->dest()));
			if (dest.isNull())
				return nullptr;
			const SemanticTree::Expr *val = convertExpression(node->value(), dest->type());
			if (val == nullptr)
				return nullptr;
			return new SemanticTree::StmtAssignment(dest.take(), val);
		}
		case SyntaxTree::NodeType::SignalEmission:
		{
			const SyntaxTree::SignalEmission *node =
				static_cast<const SyntaxTree::SignalEmission*>(statement);
			QScopedPointer<const SemanticTree::Identifier> dest(resolveIdentifier(node->signal()));
			if (dest.isNull())
				return nullptr;
			const SemanticTree::Expr *val = convertExpression(node->value(), dest->type());
			if (val == nullptr)
				return nullptr;
			return new SemanticTree::StmtAssignment(dest.take(), val);
		}
		case SyntaxTree::NodeType::IfElse:
		{
			const SyntaxTree::IfElse *node =
				static_cast<const SyntaxTree::IfElse*>(statement);
			break;
		}
		case SyntaxTree::NodeType::ChoiceOr:
		{
			const SyntaxTree::ChoiceOr *node =
				static_cast<const SyntaxTree::ChoiceOr*>(statement);
			break;
		}
		case SyntaxTree::NodeType::Branch:
		{
			const SyntaxTree::Branch *node =
				static_cast<const SyntaxTree::Branch*>(statement);
			break;
		}
		default:
			qFatal("This should never happen");
			break;
	}
	setError(statement->location(), "Not implemented yet");
	return nullptr;
}

}
}
