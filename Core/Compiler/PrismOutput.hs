-- Copyright (C) 2016 Fabio D'Urso
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

module PrismOutput where

import SemanticTree

data PrismExpr =
	  PrismExprBoolLiteral Bool				-- bool literal
	| PrismExprIntLiteral Int				-- int literal
	| PrismExprVariable String				-- variable to read
	| PrismExprBinOp String PrismExpr PrismExpr		-- binary operator
	| PrismExprNotOp PrismExpr				-- not operator
	| PrismExprUnProp Char Char PrismExpr			-- unary property
	| PrismExprBinProp Char Char PrismExpr PrismExpr	-- binary property

formatPrismQuantifier :: Char -> String
formatPrismQuantifier 'A' = "$forall$"
formatPrismQuantifier 'E' = "$exists$"

formatPrismExpr :: PrismExpr -> String
formatPrismExpr (PrismExprBoolLiteral True) = "true"
formatPrismExpr (PrismExprBoolLiteral False) = "false"
formatPrismExpr (PrismExprIntLiteral val) = show val
formatPrismExpr (PrismExprVariable var) = var
formatPrismExpr (PrismExprBinOp op e1 e2) = "(" ++ (formatPrismExpr e1) ++ op ++ (formatPrismExpr e2) ++ ")"
formatPrismExpr (PrismExprNotOp e) = "!" ++ (formatPrismExpr e)
formatPrismExpr (PrismExprUnProp q o a) = formatPrismQuantifier q ++ " [ " ++ o:' ':(formatPrismExpr a) ++ " ]"
formatPrismExpr (PrismExprBinProp q o a b) = formatPrismQuantifier q ++ " [ " ++ (formatPrismExpr a) ++ ' ':o:' ':(formatPrismExpr b) ++ " ]"

data PrismGlobalVarDecl =
	  PrismGlobalVarDeclBool String PrismExpr	-- boolean global variable with a given initial value
	| PrismGlobalVarDeclInt String Int Int PrismExpr -- bounded int global variable with a given initial value

formatPrismGlobalVarDecl :: PrismGlobalVarDecl -> String
formatPrismGlobalVarDecl (PrismGlobalVarDeclBool varName expr) =
	"global " ++ varName ++ " : bool init " ++ (formatPrismExpr expr)
formatPrismGlobalVarDecl (PrismGlobalVarDeclInt varName from to expr) =
	"global " ++ varName ++ " : [" ++ (show from) ++ ".." ++ (show (max to (from+1))) ++ "] init " ++ (formatPrismExpr expr)

formatPrismAssignment :: String -> PrismExpr -> String
formatPrismAssignment varName expr = "(" ++ varName ++ "'=" ++ (formatPrismExpr expr) ++ ")"
