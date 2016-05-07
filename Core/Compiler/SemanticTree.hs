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
