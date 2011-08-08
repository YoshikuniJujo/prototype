module Main where

import Object
import Adder

main :: IO ()
main = let
	( ret, var ) = runObject $ do
		( adder, setx, addx, getx ) <- makeAdder
		sendMsg adder setx [ primitiveInt 3 ]
		sendMsg adder addx [ primitiveInt 8 ]
		[ x ] <- sendMsg adder getx [ ]
		return ( x, setx ) in do
	print ret
	printVarName var
