{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings where

import SemanticTree

import Foreign.C.String
import Foreign.StablePtr

-- SemanticTree.Type
foreign export ccall hsTypeBool_create :: IO (StablePtr Type)
foreign export ccall hsTypeEnumeration_create :: IO (StablePtr Type)
foreign export ccall hsTypeEnumeration_prependValue :: StablePtr Type -> CString -> IO (StablePtr Type)
foreign export ccall hsTypeClass_create :: IO (StablePtr Type)
foreign export ccall hsTypeClass_prependMemberVariable :: StablePtr Type -> CString -> StablePtr Type -> IO (StablePtr Type)
foreign export ccall hsTypeSet_create :: StablePtr Type -> IO (StablePtr Type)
foreign export ccall hsType_dump :: StablePtr Type -> IO ()
foreign export ccall hsType_free :: StablePtr Type -> IO ()

hsTypeBool_create :: IO (StablePtr Type)
hsTypeBool_create = newStablePtr TypeBool

hsTypeEnumeration_create :: IO (StablePtr Type)
hsTypeEnumeration_create = newStablePtr (TypeEnumeration [])

hsTypeEnumeration_prependValue :: StablePtr Type -> CString -> IO (StablePtr Type)
hsTypeEnumeration_prependValue enum_ptr str_ptr = do
	TypeEnumeration vs <- deRefStablePtr enum_ptr
	v <- peekCString str_ptr
	newStablePtr (TypeEnumeration (v:vs))

hsTypeClass_create :: IO (StablePtr Type)
hsTypeClass_create = newStablePtr (TypeClass [])

hsTypeClass_prependMemberVariable :: StablePtr Type -> CString -> StablePtr Type -> IO (StablePtr Type)
hsTypeClass_prependMemberVariable class_ptr str_ptr type_ptr = do
	TypeClass vs <- deRefStablePtr class_ptr
	new_n <- peekCString str_ptr
	new_t <- deRefStablePtr type_ptr
	newStablePtr (TypeClass ((new_n, new_t):vs))

hsTypeSet_create :: StablePtr Type -> IO (StablePtr Type)
hsTypeSet_create innertype_ptr = do
	t <- deRefStablePtr innertype_ptr
	newStablePtr (TypeSet t)

hsType_dump :: StablePtr Type -> IO ()
hsType_dump x = do
	v <- deRefStablePtr x
	putStrLn (show v)

hsType_free :: StablePtr Type -> IO ()
hsType_free x = freeStablePtr x

-- SemanticTree.Idnt
foreign export ccall hsIdntGlobal_create :: CString -> StablePtr Type -> IO (StablePtr Idnt)
foreign export ccall hsIdntMember_create :: StablePtr Idnt -> CString -> StablePtr Type -> IO (StablePtr Idnt)
foreign export ccall hsIdnt_dump :: StablePtr Idnt -> IO ()
foreign export ccall hsIdnt_free :: StablePtr Idnt -> IO ()

hsIdntGlobal_create :: CString -> StablePtr Type -> IO (StablePtr Idnt)
hsIdntGlobal_create name_ptr type_ptr = do
	n <- peekCString name_ptr
	t <- deRefStablePtr type_ptr
	newStablePtr (IdntGlobal n t)

hsIdntMember_create :: StablePtr Idnt -> CString -> StablePtr Type -> IO (StablePtr Idnt)
hsIdntMember_create container_ptr name_ptr type_ptr = do
	c <- deRefStablePtr container_ptr
	n <- peekCString name_ptr
	t <- deRefStablePtr type_ptr
	newStablePtr (IdntMember c n t)

hsIdnt_dump :: StablePtr Idnt -> IO ()
hsIdnt_dump x = do
	v <- deRefStablePtr x
	putStrLn (show v)

hsIdnt_free :: StablePtr Idnt -> IO ()
hsIdnt_free x = freeStablePtr x

-- SemanticTree.Expr
foreign export ccall hsExprBoolLiteral_create :: Bool -> IO (StablePtr Expr)
foreign export ccall hsExprEnumLiteral_create :: StablePtr Type -> Int -> IO (StablePtr Expr)
foreign export ccall hsExprClassNilLiteral_create :: StablePtr Type -> IO (StablePtr Expr)
foreign export ccall hsExprVariable_create :: StablePtr Idnt -> IO (StablePtr Expr)
foreign export ccall hsExprEqOp_create :: StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)
foreign export ccall hsExprNeqOp_create :: StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)
foreign export ccall hsExprAndOp_create :: StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)
foreign export ccall hsExprOrOp_create :: StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)
foreign export ccall hsExprNotOp_create :: StablePtr Expr -> IO (StablePtr Expr)
foreign export ccall hsExprTuple_create :: StablePtr Type -> IO (StablePtr Expr)
foreign export ccall hsExprTuple_prependTerm :: StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)
foreign export ccall hsExprSetContains_create :: StablePtr Idnt -> StablePtr Expr -> IO (StablePtr Expr)
foreign export ccall hsExpr_dump :: StablePtr Expr -> IO ()
foreign export ccall hsExpr_free :: StablePtr Expr -> IO ()

hsExprBoolLiteral_create :: Bool -> IO (StablePtr Expr)
hsExprBoolLiteral_create val = newStablePtr (ExprBoolLiteral val)

hsExprEnumLiteral_create :: StablePtr Type -> Int -> IO (StablePtr Expr)
hsExprEnumLiteral_create type_ptr val = do
	t <- deRefStablePtr type_ptr
	newStablePtr (ExprEnumLiteral t val)

hsExprClassNilLiteral_create :: StablePtr Type -> IO (StablePtr Expr)
hsExprClassNilLiteral_create type_ptr = do
	t <- deRefStablePtr type_ptr
	newStablePtr (ExprClassNilLiteral t)

hsExprVariable_create :: StablePtr Idnt -> IO (StablePtr Expr)
hsExprVariable_create idnt_ptr = do
	i <- deRefStablePtr idnt_ptr
	newStablePtr (ExprVariable i)

hsExprEqOp_create :: StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)
hsExprEqOp_create e1_ptr e2_ptr = do
	e1 <- deRefStablePtr e1_ptr
	e2 <- deRefStablePtr e2_ptr
	newStablePtr (ExprEqOp e1 e2)

hsExprNeqOp_create :: StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)
hsExprNeqOp_create e1_ptr e2_ptr = do
	e1 <- deRefStablePtr e1_ptr
	e2 <- deRefStablePtr e2_ptr
	newStablePtr (ExprNeqOp e1 e2)

hsExprAndOp_create :: StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)
hsExprAndOp_create e1_ptr e2_ptr = do
	e1 <- deRefStablePtr e1_ptr
	e2 <- deRefStablePtr e2_ptr
	newStablePtr (ExprAndOp e1 e2)

hsExprOrOp_create :: StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)
hsExprOrOp_create e1_ptr e2_ptr = do
	e1 <- deRefStablePtr e1_ptr
	e2 <- deRefStablePtr e2_ptr
	newStablePtr (ExprOrOp e1 e2)

hsExprNotOp_create :: StablePtr Expr -> IO (StablePtr Expr)
hsExprNotOp_create e_ptr = do
	e <- deRefStablePtr e_ptr
	newStablePtr (ExprNotOp e)

hsExprTuple_create :: StablePtr Type -> IO (StablePtr Expr)
hsExprTuple_create t_ptr = do
	t <- deRefStablePtr t_ptr
	newStablePtr (ExprTuple t [])

hsExprTuple_prependTerm :: StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)
hsExprTuple_prependTerm baseexpr_ptr newexpr_ptr = do
	ExprTuple t es <- deRefStablePtr baseexpr_ptr
	e <- deRefStablePtr newexpr_ptr
	newStablePtr (ExprTuple t (e:es))

hsExprSetContains_create :: StablePtr Idnt -> StablePtr Expr -> IO (StablePtr Expr)
hsExprSetContains_create idnt_ptr expr_ptr = do
	i <- deRefStablePtr idnt_ptr
	e <- deRefStablePtr expr_ptr
	newStablePtr (ExprSetContains i e)

hsExpr_dump :: StablePtr Expr -> IO ()
hsExpr_dump x = do
	v <- deRefStablePtr x
	putStrLn (show v)

hsExpr_free :: StablePtr Expr -> IO ()
hsExpr_free x = freeStablePtr x
