{-# LANGUAGE PackageImports #-}

module Greeter (
	importGreeter
) where

import Control.Prototype
import "monads-tf" Control.Monad.Trans

importGreeter :: MonadIO m => Prot m ( Object, Member, Member, Member )
importGreeter = package "greeter" $ do
	greeter <- clone object
	name		<- makeMember "name"
	initialize	<- makeMember "initialize"
	say_hi		<- makeMember "say_hi"
	say_bye		<- makeMember "say_bye"
	setMethod greeter initialize $ \this args -> do
		let n = case args of
			[ ]	-> primStr "World"
			a : _	-> a
		setMember this name n
		return [ ]
	setMethod greeter say_hi $ \this args -> do
		n <- member this name
		liftIO $ putStrLn $ "Hi " ++ fromPrimStr n ++ "!"
		return [ ]
	setMethod greeter say_bye $ \this args -> do
		n <- member this name
		liftIO $ putStrLn $ "Bye " ++ fromPrimStr n ++
			", come back soon."
		return [ ]
	return ( greeter, initialize, say_hi, say_bye )
