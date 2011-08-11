module Adder (
	makeAdder
) where

import Control.Prototype
import Data.Function

makeAdder :: Monad m => PTMonad m ( Object, Member, Member, Member )
makeAdder = package "adder" $ do
	i <- clone object
	setx <- makeMember "setx"
	getx <- makeMember "getx"
	addx <- makeMember "addx"
{-
	setMember i vnSetx mtSetx
	setMember i vnGetx mtGetx
	setMember i vnAddx mtAddx
-}
	setMethod i setx mSetx -- mkMethod mSetx
	setMethod i getx mGetx -- mkMethod mGetx
	setMethod i addx mAddx -- mkMethod mAddx
	return ( i, setx, addx, getx )

mSetx :: Monad m => Method m
mSetx obj [ x ] = do
	vnx <- makeMember "x"
	setMember obj vnx x
	return [ ]

mAddx :: Monad m => Method m
mAddx obj [ y ] = do
	vnx <- makeMember "x"
	x <- member obj vnx
	setMember obj vnx $ primInt $ ( ( + ) `on` fromPrimInt ) x y
	return [ ]

mGetx :: Monad m => Method m
mGetx obj [ ] = do
	vnx <- makeMember "x"
	ret <- member obj vnx
	return [ ret ]
