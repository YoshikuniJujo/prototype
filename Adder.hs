module Adder (
	makeAdder
) where

import Object
import Data.Function

makeAdder :: Monad m => ObjectMonad m ( ObjectId, VarName, VarName, VarName )
makeAdder = do
	i <- clone object
	vnSetx <- mkVarName "setx"
	vnGetx <- mkVarName "getx"
	vnAddx <- mkVarName "addx"
	mtSetx <- mkMethod mSetx
	mtGetx <- mkMethod mGetx
	mtAddx <- mkMethod mAddx
	setVar i vnSetx mtSetx
	setVar i vnGetx mtGetx
	setVar i vnAddx mtAddx
	return ( i, vnSetx, vnAddx, vnGetx )

mSetx :: Monad m => Method m
mSetx obj [ x ] = do
	vnx <- mkVarName "x"
	setVar obj vnx x
	return [ ]

mAddx :: Monad m => Method m
mAddx obj [ y ] = do
	vnx <- mkVarName "x"
	x <- getVar obj vnx
	setVar obj vnx $ primitiveInt $ ( ( + ) `on` fromPrimitiveInt ) x y
	return [ ]

mGetx :: Monad m => Method m
mGetx obj [ ] = do
	vnx <- mkVarName "x"
	ret <- getVar obj vnx
	return [ ret ]
