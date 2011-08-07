{-# LANGUAGE PackageImports #-}

module Object (
	ObjectMonad,
	ObjectMethod,
	ObjectEnv,
	runObject,
	getMethodName,
	getObjectType,
	setObjectMethod,
	newObject,
	setObjectValue,
	getObjectValueName,

	ObjectValue( .. ),

	Identity,
	runIdentity
) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Identity
import Data.Maybe

type ObjectMonad m = StateT ( ObjectEnv m ) m

type ObjectMethod m = ObjectPtr -> [ ObjectPtr ] -> ObjectMonad m [ ObjectPtr ]

data ObjectEnv m = ObjectEnv {
	objectCount	:: Integer,
	objects		:: [ ( ObjectPtr, Object ) ],
	methods		:: [ ( ( ObjectType, MethodName ), ObjectMethod m ) ]
 }

instance Show ( ObjectEnv m ) where
	show oe = "ObjectEnv { objectCount = " ++ show ( objectCount oe ) ++
		", objects = " ++ show ( objects oe ) ++ ", methods = ... }"

data ObjectPtr	= ObjectPtr Integer deriving ( Eq, Show )
data ObjectType	= ObjectType String deriving ( Eq, Show )
data MethodName	= MethodName String deriving ( Eq, Show )

data Object = Object {
	objectType	:: ObjectType,
	objectData	:: [ ( ObjectValueName, ObjectValue ) ]
 } deriving Show

data ObjectValue = ObjectValueBool Bool | ObjectValueInt Int | ObjectValueString String deriving Show
data ObjectValueName = ObjectValueName String deriving ( Eq, Show )

getObjectValueName :: Monad m => String -> ObjectMonad m ObjectValueName
getObjectValueName vn = return $ ObjectValueName vn

initObjectEnv :: ObjectEnv m
initObjectEnv = ObjectEnv {
	objectCount	= 0,
	objects		= [ ],
	methods		= [ ]
 }

runObject :: ObjectMonad m a -> m ( a, ObjectEnv m )
runObject = flip runStateT initObjectEnv

getObjectType :: Monad m => String -> ObjectMonad m ObjectType
getObjectType tn = return $ ObjectType tn

getMethodName :: Monad m => String -> ObjectMonad m MethodName
getMethodName mn = return $ MethodName mn

setObjectMethod :: Monad m => ObjectType -> MethodName -> ObjectMethod m -> ObjectMonad m ()
setObjectMethod t n m = do
	oe@ObjectEnv { methods = ms } <- get
	put oe { methods = ( ( t, n ), m ) : ms }

newObject :: Monad m => ObjectType -> {- [ Value ] -> -} ObjectMonad m ObjectPtr
newObject t = do
	oe@ObjectEnv { objectCount = oc, objects = os } <- get
	put $ oe { objectCount = oc + 1, objects = ( ObjectPtr oc, Object t [ ] ) : os }
	return $ ObjectPtr oc

setObjectValue :: Monad m => ObjectPtr -> ObjectValueName -> ObjectValue -> ObjectMonad m ()
setObjectValue ptr vn v = do
	oe@ObjectEnv { objects = os } <- get
	let Object t vs = fromJust $ lookup ptr os
	put oe { objects = ( ptr, Object t ( ( vn, v ) : vs ) ) : os }
