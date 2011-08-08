{-# LANGUAGE PackageImports #-}

module Object (
	ObjectMonad,
	ObjectId,
	VarName,
	printVarName,
	fromPrimitiveInt,
	primitiveInt,
	Method,
	object,
	runObject,
	clone,
	mkVarName,
	setVar,
	getVar,
	mkMethod,
	sendMsg
) where

import "monads-tf" Control.Monad.State
import Data.List
import Data.Maybe

data Object =
	Object {
		objectId	:: ObjectId,
		objectStatus	:: [ ( VarName, ObjectId ) ] } |
	Method {
		objectId :: ObjectId,
		method :: Method }

data ObjectId =
	ObjectId { objectIdInt :: Int	} |
	PrimitiveInt { fromPrimitiveInt :: Int }
	deriving ( Eq, Show )

primitiveInt :: Int -> ObjectId
primitiveInt = PrimitiveInt

type Method = ObjectId -> [ ObjectId ] -> ObjectMonad [ ObjectId ]

object :: ObjectId
object = ObjectId 0

initObject :: Object
initObject = Object object [ ]

instance Show Object where
	show Method { }		= "method"
	show ( Object id st )	= "Object " ++ show id ++ " " ++ show st
data VarName = VarName String deriving ( Eq, Show )

printVarName :: VarName -> IO ()
printVarName ( VarName vn ) = putStrLn vn

type ObjectMonad = State ObjectEnv

type ObjectEnv = [ Object ]

initObjectEnv :: ObjectEnv
initObjectEnv = [ initObject ]

debugObject :: ObjectMonad a -> ( a, ObjectEnv )
debugObject = flip runState initObjectEnv

runObject :: ObjectMonad a -> a
runObject = flip evalState initObjectEnv

getObject :: ObjectId -> ObjectMonad Object
getObject obj = do
	gets ( head . filter ( ( == obj ) . objectId ) )

putObject :: Object -> ObjectMonad ()
putObject = modify . ( : )

getNewId :: ObjectMonad ObjectId
getNewId = do
	ids <- gets $ map ( objectIdInt . objectId )
	return $ ObjectId $ head $ [ 1 .. ] \\ ids

clone :: ObjectId -> ObjectMonad ObjectId
clone obj = do
	st <- fmap objectStatus $ getObject obj
	newId <- getNewId
	putObject $ Object newId st
	return newId

mkVarName :: String -> ObjectMonad VarName
mkVarName = return . VarName

setVar :: ObjectId -> VarName -> ObjectId -> ObjectMonad ()
setVar obj vn val = do
	st <- fmap objectStatus $ getObject obj
	putObject $ Object obj $ ( vn, val ) : st

getVar :: ObjectId -> VarName -> ObjectMonad ObjectId
getVar obj vn = do
	st <- fmap objectStatus $ getObject obj
	return $ fromJust $ lookup vn st

mkMethod :: Method -> ObjectMonad ObjectId
mkMethod m = do
	newId <- getNewId
	putObject $ Method newId m
	return newId

sendMsg :: ObjectId -> VarName -> [ ObjectId ] -> ObjectMonad [ ObjectId ]
sendMsg obj mn args = do
	o@Object { objectStatus = vs } <- getObject obj
	let	mt = fromJust $ lookup mn vs
	Method _ m <- getObject mt
	m obj args
