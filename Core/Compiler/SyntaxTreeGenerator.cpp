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

#include "Core/Compiler/SyntaxTreeGenerator.h"

#include "Core/Compiler/Lexer.h"
#include "Core/Compiler/Parser.h"

#include <QDebug>

#include <sstream>

namespace Core
{
namespace Compiler
{

SyntaxTreeGenerator::SyntaxTreeGenerator(const QString &sourceCode, SourceType type)
: m_success(true), m_resultScript(nullptr), m_resultValue(nullptr)
{
	std::istringstream is(sourceCode.toStdString());
	Lexer lexer(&is);

	// Select start symbol
	switch (type)
	{
		case Script:
			lexer.injectToken(Core::Compiler::Parser::token::START_SCRIPT);
			break;
		case Value:
		case Property:
			lexer.injectToken(Core::Compiler::Parser::token::START_VALUE);
			break;
	}

	Parser parser(lexer, this, type == Property);
	//parser.set_debug_level(true);
	if (parser.parse() != 0 && m_success)
		setError(SourceLocation(), "Unknown error");
}

SyntaxTreeGenerator::~SyntaxTreeGenerator()
{
	while (m_allNodes.isEmpty() == false)
	{
		SyntaxTree::Node *node = *m_allNodes.begin();
		//qDebug() << "GCing" << node;
		m_allNodes.remove(node);
		delete node;
	}
}

bool SyntaxTreeGenerator::success() const
{
	return m_success;
}

const SourceLocation &SyntaxTreeGenerator::errorLocation() const
{
	Q_ASSERT(m_success == false);
	return m_errorLocation;
}

const QString &SyntaxTreeGenerator::errorMessage() const
{
	Q_ASSERT(m_success == false);
	return m_errorMessage;
}

SyntaxTree::Statement *SyntaxTreeGenerator::resultScript()
{
	Q_ASSERT(m_success == true);
	return m_resultScript;
}

SyntaxTree::Expression *SyntaxTreeGenerator::resultValue()
{
	Q_ASSERT(m_success == true);
	return m_resultValue;
}

void SyntaxTreeGenerator::setError(const SourceLocation &location, const QString &message)
{
	// setError must not be called more than once
	Q_ASSERT(m_success == true);

	m_errorLocation = location;
	m_errorMessage = message;
	m_success = false;

	//qDebug() << "SyntaxTreeGenerator failed at" << location.toString() << ":" << message;
}

void SyntaxTreeGenerator::setResultScript(SyntaxTree::Statement *expr)
{
	// setError must not have been called
	Q_ASSERT(m_success == true);

	//qDebug() << "Got resultScript:" << expr->toString().toLatin1().constData();
	m_resultScript = expr;
}

void SyntaxTreeGenerator::setResultValue(SyntaxTree::Expression *expr)
{
	//qDebug() << "Got resultValue:" << expr->toString().toLatin1().constData();
	m_resultValue = expr;
}

}
}
