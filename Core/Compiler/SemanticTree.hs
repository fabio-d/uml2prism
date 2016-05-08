{-# LANGUAGE DeriveDataTypeable #-}
module SemanticTree where

import Data.Data
import Data.List
import Data.Generics.Uniplate.Data

data Type =
	  TypeBool
	| TypeEnumeration [String]	-- arg is list of allowed values ("nil" is implicitly allowed too)
	| TypeClass [(String, Type)]	-- list contains (var_name, var_type) pairs for each member variable
	| TypeSet Type			-- arg is inner type
	deriving (Show, Data, Typeable)

data Identifier =
	  GlobalIdentifier String Type
	| MemberIdentifier Identifier String Type
	deriving (Data, Typeable)

--instance Show Identifier where
--	show (GlobalIdentifier name _) = name
--	show (MemberIdentifier parent name _) = (show parent) ++ "." ++ name

data Expr =
	  ExprBoolLiteral Bool		-- bool literal
	| ExprEnumLiteral Type Int	-- literal for a given TypeEnumeration (arg is 1-based index, 0 means nil)
	| ExprClassNilLiteral Type	-- nil literal for a given TypeClass
	| ExprVariable Identifier	-- variable being read (any type)
	| ExprEqOp Expr Expr		-- equal operator (any type)
	| ExprNeqOp Expr Expr		-- not equal operator (any type)
	| ExprAndOp Expr Expr		-- boolean and operator
	| ExprOrOp Expr Expr		-- boolean or operator
	| ExprNotOp Expr		-- boolean not operator
	| ExprTuple Type [Expr]		-- either a non-nil TypeClass value or a TypeSet (set of values)
	| ExprSetContains Identifier Expr -- test whether a set (first arg) contains a given element (second arg)
	deriving (Data, Typeable)
