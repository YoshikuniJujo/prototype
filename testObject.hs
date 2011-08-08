module Main where

import AddSub

main :: IO ()
main = test2

test1 :: IO ()
test1 = let
	( ret, var ) = runIdentity $ runObject $ do
		( adder, setx, getx, addx, subx ) <- makeAddSub
		sendMsg adder setx [ primitiveInt 3 ]
		sendMsg adder addx [ primitiveInt 8 ]
		[ x ] <- sendMsg adder getx [ ]
		return ( x, setx ) in do
	print ret
	printVarName var

test2 :: IO ()
test2 = runObject $ do
	( adder, setx, getx, addx, subx ) <- makeAddSub
	lift $ printVarName setx
	sendMsg adder setx [ primitiveInt 3 ]
	sendMsg adder addx [ primitiveInt 15 ]
	sendMsg adder subx [ primitiveInt 54 ]
	[ x ] <- sendMsg adder getx [ ]
	lift $ print $ fromPrimitiveInt x
