{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings where

hsHelloWorld :: IO ()
hsHelloWorld = putStrLn "Hello Haskell"

foreign export ccall
	hsHelloWorld :: IO ()
