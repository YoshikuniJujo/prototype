module Main where

import Control.Prototype
import Greeter

main :: IO ()
main = fmap fst $ flip runProt initProtEnv $ do
	( greeter, initialize, say_hi, say_bye ) <- importGreeter
	printMember say_hi
	world	<- clone greeter
	me	<- clone greeter
	method world initialize [ ]
	method me initialize [ primStr "Yoshikuni Jujo" ]
	method world say_hi [ ]
	method me say_hi [ ]
	method me say_bye [ ]
	return ()
