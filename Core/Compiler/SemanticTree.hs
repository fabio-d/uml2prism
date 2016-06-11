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
	deriving (Data, Typeable)

instance Show Type where
	show TypeBool = "bool"
	show (TypeEnumeration values) = "enum(" ++ (intercalate "," values) ++ ")"
	show (TypeClass members) = "class(" ++ (intercalate "," [ n ++ ":" ++ show t | (n, t) <- members]) ++ ")"
	show (TypeSet t) = "set of " ++ (show t)

data Idnt =
	  IdntGlobal String Type	-- global variable with given name and type
	| IdntMember Idnt String Type	-- member variable with given name and type (contained inside a variable)
	deriving (Data, Typeable)

instance Show Idnt where
	show (IdntGlobal name _) = name
	show (IdntMember parent name _) = (show parent) ++ "." ++ name

data Expr =
	  ExprBoolLiteral Bool		-- bool literal
	| ExprEnumLiteral Type Int	-- literal for a given TypeEnumeration (arg is 1-based index, 0 means nil)
	| ExprClassNilLiteral Type	-- nil literal for a given TypeClass
	| ExprVariable Idnt		-- variable being read (any type)
	| ExprEqOp Expr Expr		-- equal operator (any type)
	| ExprNeqOp Expr Expr		-- not equal operator (any type)
	| ExprAndOp Expr Expr		-- boolean and operator
	| ExprOrOp Expr Expr		-- boolean or operator
	| ExprImpliesOp Expr Expr	-- boolean implies operator
	| ExprIffOp Expr Expr		-- boolean if-and-only-if operator
	| ExprNotOp Expr		-- boolean not operator
	| ExprUnProp Char Char Expr	-- unary property
	| ExprBinProp Char Char Expr Expr -- binary property
	| ExprTuple Type [Expr]		-- either a non-nil TypeClass value or a TypeSet (set of values)
	| ExprSetContains Idnt Expr	-- test whether a set (first arg) contains a given element (second arg)
	deriving (Data, Typeable)

instance Show Expr where
	show (ExprBoolLiteral True) = "true"
	show (ExprBoolLiteral False) = "false"
	show (ExprEnumLiteral _ 0) = "nil"
	show (ExprEnumLiteral (TypeEnumeration values) idx) = values!!(idx - 1)
	show (ExprClassNilLiteral _) = "nil"
	show (ExprVariable idnt) = show idnt
	show (ExprEqOp a b) = "(" ++ (show a) ++ " == " ++ (show b) ++ ")"
	show (ExprNeqOp a b) = "(" ++ (show a) ++ " != " ++ (show b) ++ ")"
	show (ExprAndOp a b) = "(" ++ (show a) ++ " && " ++ (show b) ++ ")"
	show (ExprOrOp a b) = "(" ++ (show a) ++ " || " ++ (show b) ++ ")"
	show (ExprImpliesOp a b) = "(" ++ (show a) ++ " => " ++ (show b) ++ ")"
	show (ExprIffOp a b) = "(" ++ (show a) ++ " <=> " ++ (show b) ++ ")"
	show (ExprNotOp a) = "!" ++ (show a)
	show (ExprUnProp q o a) = q:" [ " ++ o:' ':(show a) ++ " ]"
	show (ExprBinProp q o a b) = q:" [ " ++ (show a) ++ ' ':o:' ':(show b) ++ " ]"
	show (ExprTuple _ vs) = "{" ++ (intercalate "," [show x | x <- vs]) ++ "}"
	show (ExprSetContains idnt v) = show idnt ++ ".contains(" ++ show v ++ ")"

data Stmt =
	  StmtCompound [Stmt]
	| StmtSetInsert Idnt Expr
	| StmtAssignment Idnt Expr
	| StmtIfElse Expr Stmt Stmt
	| StmtChoiceOr Stmt Stmt
	| StmtBranch String		-- arg is next node's name
	deriving (Show, Data, Typeable)
