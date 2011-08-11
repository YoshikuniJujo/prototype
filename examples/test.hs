module Main where

import Control.Prototype

main = flip runPT initPTEnv $ do
	( dog, setName, sit ) <- package "dog" $ do
		honyuurui <- clone object
		dog <- clone honyuurui
		sit <- makeMember "sit"
		setName <- makeMember "setName"
		name <- makeMember "name"
		setMethod dog sit $ \obj _ -> do
			n <- member obj name
			liftPT $ putStrLn $ fromPrimStr n ++ " sitting."
			return [ ]
		setMethod dog setName $ \obj [ n ] -> do
			setMember obj name n
			return [ ]
		return ( dog, setName, sit )
	myDog <- clone dog
	method myDog setName [ primStr "John" ]
	method myDog sit [ ]
