module Main where

import AddSub

main :: IO ()
main = test2

test1 :: IO ()
test1 = let
	( ( ret, var ), _ ) = runIdentity $ flip runProt initProtEnv $ do
		( adder, setx, getx, addx, subx ) <- makeAddSub
		method adder setx [ primInt 3 ]
		method adder addx [ primInt 8 ]
		[ x ] <- method adder getx [ ]
		return ( x, setx ) in do
	print ret
	printMember var

test2 :: IO ()
test2 = fmap fst $ flip runProt initProtEnv $ do
	( adder, setx, getx, addx, subx ) <- makeAddSub
	liftProt $ printMember setx
	method adder setx [ primInt 3 ]
	method adder addx [ primInt 15 ]
	method adder subx [ primInt 54 ]
	[ x ] <- method adder getx [ ]
	liftProt $ print $ fromPrimInt x
