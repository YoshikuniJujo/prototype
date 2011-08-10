module Main where

import Control.Prototype

main = flip runPT initPTEnv $ do
	honyuurui <- clone object
	dog <- clone honyuurui
	sit <- makeMember "sit"
	setName <- makeMember "setName"
	setMethod dog sit $ \obj _ -> do
		name <- makeMember "name"
		n <- member obj name
		liftPT $ putStrLn $ fromPrimStr n ++ " sitting."
		return [ ]
	setMethod dog setName $ \obj [ n ] -> do
		name <- makeMember "name"
		setMember obj name n
		return [ ]
	myDog <- clone dog
	method myDog setName [ primStr "John" ]
	method myDog sit [ ]
