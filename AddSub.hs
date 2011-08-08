module AddSub (
	makeAddSub,

	runObject,
	sendMsg,
	lift,
	runIdentity,
	primitiveInt,
	fromPrimitiveInt,
	printVarName
) where

import Adder
import Object
import Data.Function

makeAddSub :: Monad m =>
	ObjectMonad m ( ObjectId, VarName, VarName, VarName, VarName )
makeAddSub = do
	( adder, setx, addx, getx ) <- makeAdder
	suber <- clone adder
	subx <- mkVarName "subx"
	mtSubx <- mkMethod mSubx
	setVar suber subx mtSubx
	return ( suber, setx, getx, addx, subx )

mSubx :: Monad m => Method m
mSubx obj [ y ] = do
	vnx <- mkVarName "x"
	x <- getVar obj vnx
	setVar obj vnx $ primitiveInt $ ( ( - ) `on` fromPrimitiveInt ) x y
	return [ ]
