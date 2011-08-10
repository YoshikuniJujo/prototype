{-# LANGUAGE PackageImports #-}

module AddSub (
	makeAddSub,

	runPT,
	initPTEnv,
	method,
	liftPT,
	runIdentity,
	primInt,
	fromPrimInt,
	printMemberName
) where

import Adder
import Control.Prototype
import Data.Function
import "monads-tf" Control.Monad.Identity

makeAddSub :: Monad m =>
	PTMonad m ( Object, Member, Member, Member, Member )
makeAddSub = do
	( adder, setx, addx, getx ) <- makeAdder
	suber <- clone adder
	subx <- makeMember "subx"
{-
	mtSubx <- mkMethod mSubx
	setMember suber subx mtSubx
-}
	setMethod suber subx mSubx
	return ( suber, setx, getx, addx, subx )

mSubx :: Monad m => Method m
mSubx obj [ y ] = do
	vnx <- makeMember "x"
	x <- member obj vnx
	setMember obj vnx $ primInt $ ( ( - ) `on` fromPrimInt ) x y
	return [ ]
