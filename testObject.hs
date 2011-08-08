module Main where

import Object

main :: IO ()
main = do
	print $ runObject testObject

testObject :: ObjectMonad ObjectId
testObject = do
	i <- clone object
	setx <- getMethodName "setx"
--	add3 <- getMethodName "add3"
	setMethod i setx ( Method mSetx )
	sendMessage i setx [ primitiveInt 8 ]
	return i

mSetx :: ObjectId -> [ Object ] -> ObjectMonad [ Object ]
mSetx obj args = do
	return [ ]
