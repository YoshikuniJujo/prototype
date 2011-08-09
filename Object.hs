{-# LANGUAGE PackageImports #-}

module Object (
	lift,
	runIdentity,
	ObjectMonad,
	ObjectId,
	VarName,
	printVarName,
	fromPrimitiveInt,
	primitiveInt,
	fromPrimitiveString,
	primitiveString,
	Method,
	object,
	runObject,
	initObjectEnv,
	clone,
	mkVarName,
	setVar,
	getVar,
	mkMethod,
	sendMsg,
	setMethod
) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Identity
import Data.List
import Data.Maybe

setMethod :: Monad m => ObjectId -> String -> Method m -> ObjectMonad m VarName
setMethod obj mn m = do
	vn <- mkVarName mn
	mt <- mkMethod m
	setVar obj vn mt
	return vn

data Object m =
	Object {
		objectId	:: ObjectId,
		objectStatus	:: [ ( VarName, ObjectId ) ] } |
	Method {
		objectId :: ObjectId,
		method :: Method m }

data ObjectId =
	ObjectId { objectIdInt :: Int	} |
	PrimitiveInt { fromPrimitiveInt :: Int } |
	PrimitiveString { fromPrimitiveString :: String }
	deriving ( Eq, Show )

primitiveInt :: Int -> ObjectId
primitiveInt = PrimitiveInt

primitiveString :: String -> ObjectId
primitiveString = PrimitiveString

type Method m = ObjectId -> [ ObjectId ] -> ObjectMonad m [ ObjectId ]

object :: ObjectId
object = ObjectId 0

initObject :: Object m
initObject = Object object [ ]

instance Show ( Object m ) where
	show Method { }		= "method"
	show ( Object id st )	= "Object " ++ show id ++ " " ++ show st
data VarName = VarName String deriving ( Eq, Show )

printVarName :: VarName -> IO ()
printVarName ( VarName vn ) = putStrLn vn

type ObjectMonad m = StateT ( ObjectEnv m ) m

type ObjectEnv m = [ Object m ]

initObjectEnv :: ObjectEnv m
initObjectEnv = [ initObject ]

debugObject :: ObjectMonad m a -> m ( a, ObjectEnv m )
debugObject = flip runStateT initObjectEnv

runObject :: Monad m => ObjectMonad m a -> ObjectEnv m -> m ( a, ObjectEnv m )
runObject = runStateT

getObject :: Monad m => ObjectId -> ObjectMonad m ( Object m )
getObject obj = do
	ret <- gets ( hd . filter ( ( == obj ) . objectId ) )
	case ret of
		Nothing -> get >>= error . show
		Just ret -> return ret
	where
	hd [ ] = Nothing
	hd xs = Just $ head xs

putObject :: Monad m => Object m -> ObjectMonad m ()
putObject = modify . ( : )

getNewId :: Monad m => ObjectMonad m ObjectId
getNewId = do
	ids <- gets $ map ( objectIdInt . objectId )
	return $ ObjectId $ head $ [ 1 .. ] \\ ids

clone :: Monad m => ObjectId -> ObjectMonad m ObjectId
clone obj = do
	st <- getObject obj >>= return . objectStatus
	newId <- getNewId
	putObject $ Object newId st
	return newId

mkVarName :: Monad m => String -> ObjectMonad m VarName
mkVarName = return . VarName

setVar :: Monad m => ObjectId -> VarName -> ObjectId -> ObjectMonad m ()
setVar obj vn val = do
	st <- getObject obj >>= return . objectStatus
	putObject $ Object obj $ ( vn, val ) : st

getVar :: Monad m => ObjectId -> VarName -> ObjectMonad m ObjectId
getVar obj vn = do
	st <- getObject obj >>= return . objectStatus
	return $ fromJust $ lookup vn st

mkMethod :: Monad m => Method m -> ObjectMonad m ObjectId
mkMethod m = do
	newId <- getNewId
	putObject $ Method newId m
	return newId

sendMsg :: Monad m =>
	ObjectId -> VarName -> [ ObjectId ] -> ObjectMonad m [ ObjectId ]
sendMsg obj mn args = do
	o@Object { objectStatus = vs } <- getObject obj
	let	mt = fromJust $ lookup mn vs
	Method _ m <- getObject mt
	m obj args
