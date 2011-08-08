{-# LANGUAGE PackageImports #-}

module Object (
	Object,
	ObjectId,
	object,
	primitiveInt,
	Method( Method ),
	ObjectMonad,
	runObject,
	clone,
	getMethodName,
	setMethod,
	sendMessage
) where

import "monads-tf" Control.Monad.State
import Data.List
import Data.Maybe

data Object = Object {
	objectId	:: ObjectId,
	objectStatus	:: [ Object ],
	objectMethods	:: [ ( MethodName, Method ) ]
 } | PrimitiveInt Int
	deriving Show

data ObjectId = ObjectId { objectIdInt :: Int } deriving ( Eq, Show )

object :: Object
object = Object ( ObjectId 0 ) [ ] [ ]

primitiveInt :: Int -> Object
primitiveInt = PrimitiveInt

type ObjectMonad = State ObjectEnv

data ObjectEnv = ObjectEnv { envObjects :: [ Object ] } deriving Show

initObjectEnv :: ObjectEnv
initObjectEnv = ObjectEnv [ object ]

getNewId :: ObjectMonad ObjectId
getNewId = do
	ids <- gets ( map ( objectIdInt . objectId ) . envObjects )
	return $ ObjectId $ head $ [ 1 .. ] \\ ids

putNewObject :: Object -> ObjectMonad ()
putNewObject obj = do
	oe@ObjectEnv { envObjects = objs } <- get
	put $ oe { envObjects = obj : objs }

getObject :: ObjectId -> ObjectMonad Object
getObject obj = do
	objs <- gets envObjects
	return $ head $ filter ( ( == obj ) . objectId ) objs

putObject :: Object -> ObjectMonad ()
putObject obj = do
	oe@ObjectEnv { envObjects = objs } <- get
	put oe { envObjects = obj : objs }

data MethodName = MethodName String deriving ( Eq, Show )

data Method = Method ( ObjectId -> [ Object ] -> ObjectMonad [ Object ] )

instance Show Method where
	show _ = "method"

runObject :: ObjectMonad a -> (a, ObjectEnv )
runObject = flip runState initObjectEnv

clone :: Object -> ObjectMonad ObjectId
clone ( Object _ st mt ) = do
	newId <- getNewId
	putNewObject $ Object newId st mt
	return newId

getMethodName :: String -> ObjectMonad MethodName
getMethodName = return . MethodName

setMethod :: ObjectId -> MethodName -> Method -> ObjectMonad ()
setMethod obj mn m = do
	obj@Object { objectMethods = mts } <- getObject obj
	putObject $ obj { objectMethods = ( mn, m ) : mts }

sendMessage :: ObjectId -> MethodName -> [ Object ] -> ObjectMonad [ Object ]
sendMessage obj mn args = do
	o@Object { objectMethods = ms } <- getObject obj
	let Method m = fromJust $ lookup mn ms
	m obj args
