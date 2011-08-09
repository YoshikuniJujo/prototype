{-# LANGUAGE PackageImports #-}

module Control.Prototype (
	PrototypeMonad,
	Object,
	VarName,
	Method,

	runPrototype,
	initPrototypeEnv,
	object,
	clone,
	makeVarName,
	setVar,
	getVar,
	getMethod,
	sendMsg,
	setMethod,

	liftPT,
	fromPrimitiveInt,
	primitiveInt,
	fromPrimitiveString,
	primitiveString,

	printVarName,
) where

import "monads-tf" Control.Monad.State
import Data.List
import Data.Maybe

runPrototype :: Monad m =>
	PrototypeMonad m a -> ObjectEnv m -> m ( a, ObjectEnv m )
runPrototype = runObject

initPrototypeEnv :: ObjectEnv m
initPrototypeEnv = initObjectEnv

type PrototypeMonad m = ObjectMonad m

type Object = ObjectId

liftPT :: Monad m => m a -> ObjectMonad m a
liftPT = lift

makeVarName :: Monad m => String -> ObjectMonad m VarName
makeVarName = mkVarName

getMethod :: Monad m =>
	ObjectId -> VarName -> [ ObjectId ] -> ObjectMonad m [ ObjectId ]
getMethod = sendMsg

setMethod :: Monad m => ObjectId -> String -> Method m -> ObjectMonad m VarName
setMethod obj mn m = do
	vn <- mkVarName mn
	mt <- mkMethod m
	setVar obj vn mt
	return vn

data ObjectBody m =
	Object {
		objectId	:: ObjectId,
		objectStatus	:: [ ( VarName, ObjectId ) ] } |
	Method {
		objectId :: ObjectId,
		_method :: Method m }

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

initObject :: ObjectBody m
initObject = Object object [ ]

instance Show ( ObjectBody m ) where
	show Method { }		= "method"
	show ( Object oid st )	= "Object " ++ show oid ++ " " ++ show st
data VarName = VarName String deriving ( Eq, Show )

printVarName :: VarName -> IO ()
printVarName ( VarName vn ) = putStrLn vn

type ObjectMonad m = StateT ( ObjectEnv m ) m

type ObjectEnv m = [ ObjectBody m ]

initObjectEnv :: ObjectEnv m
initObjectEnv = [ initObject ]

runObject :: Monad m => ObjectMonad m a -> ObjectEnv m -> m ( a, ObjectEnv m )
runObject = runStateT

getObject :: Monad m => ObjectId -> ObjectMonad m ( ObjectBody m )
getObject obj = do
	ret <- gets ( hd . filter ( ( == obj ) . objectId ) )
	case ret of
		Nothing -> get >>= error . show
		Just x -> return x
	where
	hd [ ] = Nothing
	hd xs = Just $ head xs

putObject :: Monad m => ObjectBody m -> ObjectMonad m ()
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
	Object { objectStatus = vs } <- getObject obj
	let	mt = fromJust $ lookup mn vs
	Method _ m <- getObject mt
	m obj args
