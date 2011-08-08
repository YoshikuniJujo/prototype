module Main where

import Object
import Adder

main :: IO ()
main = print $ runObject $ do
	( adder, setx, addx, getx ) <- makeAdder
	sendMsg adder setx [ primitiveInt 3 ]
	sendMsg adder addx [ primitiveInt 8 ]
	sendMsg adder getx [ ]
