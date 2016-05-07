{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings where

import SemanticTree

import Foreign.C.String
import Foreign.StablePtr

foreign export ccall hsTypeBool_create :: IO (StablePtr Type)
foreign export ccall hsTypeEnumeration_create :: IO (StablePtr Type)
foreign export ccall hsTypeEnumeration_registerValue :: StablePtr Type -> CString -> IO (StablePtr Type)
foreign export ccall hsTypeClass_create :: IO (StablePtr Type)
foreign export ccall hsTypeClass_registerMemberVariable :: StablePtr Type -> CString -> StablePtr Type -> IO (StablePtr Type)
foreign export ccall hsTypeSet_create :: StablePtr Type -> IO (StablePtr Type)
foreign export ccall hsType_dump :: StablePtr Type -> IO ()
foreign export ccall hsType_free :: StablePtr Type -> IO ()

hsTypeBool_create :: IO (StablePtr Type)
hsTypeBool_create = newStablePtr TypeBool

hsTypeEnumeration_create :: IO (StablePtr Type)
hsTypeEnumeration_create = newStablePtr (TypeEnumeration [])

hsTypeEnumeration_registerValue :: StablePtr Type -> CString -> IO (StablePtr Type)
hsTypeEnumeration_registerValue enum_ptr str_ptr = do
	TypeEnumeration vs <- deRefStablePtr enum_ptr
	new_v <- peekCString str_ptr
	newStablePtr (TypeEnumeration (vs ++ [new_v]))

hsTypeClass_create :: IO (StablePtr Type)
hsTypeClass_create = newStablePtr (TypeClass [])

hsTypeClass_registerMemberVariable :: StablePtr Type -> CString -> StablePtr Type -> IO (StablePtr Type)
hsTypeClass_registerMemberVariable class_ptr str_ptr type_ptr = do
	TypeClass vs <- deRefStablePtr class_ptr
	new_n <- peekCString str_ptr
	new_t <- deRefStablePtr type_ptr
	newStablePtr (TypeClass (vs ++ [(new_n, new_t)]))

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
