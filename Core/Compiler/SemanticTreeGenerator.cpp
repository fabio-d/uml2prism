#include "Core/Compiler/SemanticTreeGenerator.h"

#include "Core/Compiler/SemanticContext.h"
#include "Core/Compiler/SyntaxTreeGenerator.h"

#include <QDebug>

namespace Core
{
namespace Compiler
{

SemanticTreeGenerator::SemanticTreeGenerator(const QString &sourceCode, const SemanticContext *context,
	const SemanticTree::Type *valueType, bool allowProperties)
: m_success(true), m_context(context), m_resultExpr(nullptr), m_resultStmt(nullptr)
{
	SyntaxTreeGenerator sygen(sourceCode,
		allowProperties ? SyntaxTreeGenerator::Property : SyntaxTreeGenerator::Value);

	if (!sygen.success())
	{
		setError(sygen.errorLocation(), sygen.errorMessage());
		return;
	}

	m_resultExpr = convertExpression(sygen.resultValue(), valueType);
}

SemanticTreeGenerator::SemanticTreeGenerator(const QString &sourceCode, const SemanticContext *context,
	const QStringList &writableSignals, const QMap<QString, QString> &labelMap,
	const QString &defaultBranchTarget)
: m_success(true), m_context(context), m_resultExpr(nullptr), m_resultStmt(nullptr),
  m_writableSignals(writableSignals), m_labelMap(labelMap),
  m_defaultBranchTarget(defaultBranchTarget)
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

	//qDebug() << "SemanticTreeGenerator failed at" << location.toString() << ":" << message;
}

void SemanticTreeGenerator::setUnexpectedTypeError(const SourceLocation &location, const SemanticTree::Type *expectedType, const SemanticTree::Type *actualType)
{
	setError(location,
		 QString("Expected %1 but found %2")
			.arg(expectedType->datatypeName())
			.arg(actualType->datatypeName()));
}

const SemanticTree::Idnt *SemanticTreeGenerator::resolveIdentifier(const SyntaxTree::Identifier *ident, IdentifierPurpose purpose)
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
	if (varType == nullptr && m_context->findStateOrLabelOrSignalWithoutMessage(baseVar->name()))
		varType = m_context->boolType();

	if (varType == nullptr)
	{
		setError(baseVar->location(), QString("Unresolved identifier: %1").arg(baseVar->name()));
		return nullptr;
	}

	if ((purpose == SendSignalWithoutMessage || purpose == SendSignalWithMessage) && !m_writableSignals.contains(baseVar->name()))
	{
		setError(baseVar->location(), QString("Invalid signal emission: %1 cannot be emitted by this node").arg(baseVar->name()));
		return nullptr;
	}
	else if ((purpose == SendSignalWithoutMessage && !m_context->isSignalWithoutMessage(baseVar->name()))
		|| (purpose == SendSignalWithMessage && !m_context->isSignalWithMessage(baseVar->name())))
	{
		const QString gap = purpose == SendSignalWithoutMessage ? "without" : "with";
		setError(baseVar->location(),
			 QString("Invalid signal emission: %1 is not a signal %2 attachment")
				.arg(baseVar->name())
				.arg(gap));
		return nullptr;
	}
	else if (purpose == WriteVariable && !m_context->isGlobalVariable(baseVar->name()))
	{
		setError(baseVar->location(), QString("Invalid assignment: %1 is not a variable").arg(baseVar->name()));
		return nullptr;
	}

	QScopedPointer<const SemanticTree::Idnt> res(new SemanticTree::IdntGlobal(baseVar->name(), varType));
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

		res.reset(new SemanticTree::IdntMember(res.take(), m->name(), varType));
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
			if (varType == nullptr && m_context->findStateOrLabelOrSignalWithoutMessage(node->name()))
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
			QScopedPointer<const SemanticTree::Idnt> res(resolveIdentifier(node, Read));
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
		case SyntaxTree::NodeType::UnaryProperty:
		{
			return m_context->boolType();
		}
		case SyntaxTree::NodeType::BinaryProperty:
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

			QScopedPointer<const SemanticTree::Idnt> setObject(expectSetMethod(node, Contains));
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
const SemanticTree::Idnt *SemanticTreeGenerator::expectSetMethod(const SyntaxTree::MethodCall *mCall, SetMethod method)
{
	if (mCall->object() == nullptr)
	{
		setError(mCall->location(),
			QString("No function called %1").arg(mCall->methodName()));
		return nullptr;
	}

	QScopedPointer<const SemanticTree::Idnt> object(resolveIdentifier(mCall->object(), method == Contains ? Read : WriteVariable));
	if (object.isNull())
		return nullptr;

	const SemanticTree::SetType *setType = dynamic_cast<const SemanticTree::SetType*>(object->type());
	if (setType == nullptr)
	{
		setError(mCall->location(), QString("Not a set type: %1").arg(object->type()->datatypeName()));
		return nullptr;
	}

	if (mCall->methodName() != (method == Contains ? "contains" : "insert"))
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
			if (varType == nullptr && m_context->findStateOrLabelOrSignalWithoutMessage(node->name()))
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
					if (m_context->isState(node->name()))
						return new SemanticTree::ExprStateCheck(node->name());
					else if (m_context->isLabel(node->name()))
						return new SemanticTree::ExprVariable(new SemanticTree::IdntGlobal(QString("\"%1\"").arg(node->name()), varType));
					else
						return new SemanticTree::ExprVariable(new SemanticTree::IdntGlobal(node->name(), varType));
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
			const SemanticTree::Idnt *res = resolveIdentifier(node, Read);
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
			else // Binary AND, OR, IMPLIES of IFF
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
				case SyntaxTree::BinaryOperator::Implies:
					op = SemanticTree::ExprBinOp::Implies;
					break;
				case SyntaxTree::BinaryOperator::Iff:
					op = SemanticTree::ExprBinOp::Iff;
					break;
			}

			return new SemanticTree::ExprBinOp(op, op1.take(), op2.take());
		}
		case SyntaxTree::NodeType::UnaryProperty:
		{
			const SyntaxTree::UnaryProperty *node =
				static_cast<const SyntaxTree::UnaryProperty*>(expression);

			QScopedPointer<const SemanticTree::Expr> op(
				convertExpression(node->arg(), m_context->boolType()));
			if (op.isNull())
			{
					return nullptr;
			}
			else if (expectedType == m_context->boolType())
			{
				char quantif, operand;

				switch (node->quantifier())
				{
					case SyntaxTree::PropertyQuantifier::ForAll:
						quantif = 'A';
						break;
					case SyntaxTree::PropertyQuantifier::Exists:
						quantif = 'E';
						break;
				}

				switch (node->op())
				{
					case SyntaxTree::UnaryProperty::Next:
						operand = 'X';
						break;
					case SyntaxTree::UnaryProperty::Eventually:
						operand = 'F';
						break;
					case SyntaxTree::UnaryProperty::Always:
						operand = 'G';
						break;
				}

				return new SemanticTree::ExprUnProp(quantif, operand, op.take());
			}
			else
			{
				setUnexpectedTypeError(expression->location(), expectedType, m_context->boolType());
				return nullptr;
			}
		}
		case SyntaxTree::NodeType::BinaryProperty:
		{
			const SyntaxTree::BinaryProperty *node =
				static_cast<const SyntaxTree::BinaryProperty*>(expression);

			QScopedPointer<const SemanticTree::Expr> op1(
				convertExpression(node->arg1(), m_context->boolType()));
			if (op1.isNull())
					return nullptr;
			QScopedPointer<const SemanticTree::Expr> op2(
				convertExpression(node->arg2(), m_context->boolType()));
			if (op2.isNull())
					return nullptr;

			if (expectedType != m_context->boolType())
			{
				setUnexpectedTypeError(expression->location(), expectedType, m_context->boolType());
				return nullptr;
			}

			char quantif, operand;

			switch (node->quantifier())
			{
				case SyntaxTree::PropertyQuantifier::ForAll:
					quantif = 'A';
					break;
				case SyntaxTree::PropertyQuantifier::Exists:
					quantif = 'E';
					break;
			}

			switch (node->op())
			{
				case SyntaxTree::BinaryProperty::Until:
					operand = 'U';
					break;
				case SyntaxTree::BinaryProperty::WeakUntil:
					operand = 'W';
					break;
				case SyntaxTree::BinaryProperty::Release:
					operand = 'R';
					break;
			}

			return new SemanticTree::ExprBinProp(quantif, operand, op1.take(), op2.take());
		}
		case SyntaxTree::NodeType::Tuple:
		{
			const SyntaxTree::Tuple *node =
				static_cast<const SyntaxTree::Tuple*>(expression);
			const SemanticTree::ClassType *classType =
				dynamic_cast<const SemanticTree::ClassType*>(expectedType);
			const SemanticTree::SetType *setType =
				dynamic_cast<const SemanticTree::SetType*>(expectedType);
			const QList<const SyntaxTree::Expression*> &elements = node->elements();
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

			QScopedPointer<const SemanticTree::Idnt> setObject(expectSetMethod(node, Contains));
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
	//qDebug() << "Converting statement" << statement->toString();

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

			QScopedPointer<const SemanticTree::Idnt> setObject(expectSetMethod(node, Insert));
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
			QScopedPointer<const SemanticTree::Idnt> dest(resolveIdentifier(node->dest(), WriteVariable));
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
			const bool withMessage = node->value() != nullptr;
			QScopedPointer<const SemanticTree::Idnt> dest(resolveIdentifier(node->signal(),
				withMessage ? SendSignalWithMessage : SendSignalWithoutMessage));
			if (dest.isNull())
				return nullptr;
			const SemanticTree::Expr *val =
				withMessage ? convertExpression(node->value(), dest->type())
					: new SemanticTree::ExprBoolLiteral(true);
			if (val == nullptr)
				return nullptr;
			return new SemanticTree::StmtAssignment(dest.take(), val);
		}
		case SyntaxTree::NodeType::IfElse:
		{
			const SyntaxTree::IfElse *node =
				static_cast<const SyntaxTree::IfElse*>(statement);
			QScopedPointer<const SemanticTree::Expr> cond(convertExpression(node->condition(), m_context->boolType()));
			if (cond.isNull())
				return nullptr;
			QScopedPointer<const SemanticTree::Stmt> ifTrue(convertStatement(node->trueBranch()));
			if (ifTrue.isNull())
				return nullptr;
			const SemanticTree::Stmt *ifFalse = convertStatement(node->falseBranch());
			if (ifFalse == nullptr)
				return nullptr;
			return new SemanticTree::StmtIfElse(cond.take(), ifTrue.take(), ifFalse);
		}
		case SyntaxTree::NodeType::ChoiceOr:
		{
			const SyntaxTree::ChoiceOr *node =
				static_cast<const SyntaxTree::ChoiceOr*>(statement);
			QScopedPointer<const SemanticTree::Stmt> alt1(convertStatement(node->alt1()));
			if (alt1.isNull())
				return nullptr;
			const SemanticTree::Stmt *alt2 = convertStatement(node->alt2());
			if (alt2 == nullptr)
				return nullptr;
			return new SemanticTree::StmtChoiceOr(alt1.take(), alt2);
		}
		case SyntaxTree::NodeType::Branch:
		{
			const SyntaxTree::Branch *node =
				static_cast<const SyntaxTree::Branch*>(statement);

			// only accept "branch;" (w/o label) if there is one and only one outgoing edge
			if (node->label() == "$default$")
			{
				if (m_defaultBranchTarget.isEmpty())
				{
					setError(statement->location(), "Default branch can be used only when a node has one and only one outgoing control flow edge");
					return nullptr;
				}
				else
				{
					return new SemanticTree::StmtBranch(m_defaultBranchTarget);
				}
			}
			else
			{
				if (m_labelMap.contains(node->label()))
				{
					return new SemanticTree::StmtBranch(m_labelMap[node->label()]);
				}
				else
				{
					setError(statement->location(),
						QString("Undefined label: %1")
							.arg(node->label()));
					return nullptr;
				}
			}
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
