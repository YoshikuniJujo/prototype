module Adder (
	makeAdder
) where

import Control.Prototype
import Data.Function

makeAdder :: Monad m => PTMonad m ( Object, Member, Member, Member )
makeAdder = do
	i <- clone object
{-
	vnSetx <- makeMember "setx"
	vnGetx <- makeMember "getx"
	vnAddx <- makeMember "addx"
	setMember i vnSetx mtSetx
	setMember i vnGetx mtGetx
	setMember i vnAddx mtAddx
-}
	vnSetx <- setMethod i "setx" mSetx -- mkMethod mSetx
	vnGetx <- setMethod i "getx" mGetx -- mkMethod mGetx
	vnAddx <- setMethod i "addx" mAddx -- mkMethod mAddx
	return ( i, vnSetx, vnAddx, vnGetx )

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
