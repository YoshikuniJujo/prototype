{-# LANGUAGE PackageImports #-}

module Control.Prototype (
	PTMonad,
	Object,
	Member,
	Method,

	runPT,
	initPTEnv,
	object,
	clone,
	makeMember,
	member,
	method,
	setMember,
	setMethod,

	liftPT,
	primInt,
	primStr,
	fromPrimInt,
	fromPrimStr,

	printMemberName,
) where

import "monads-tf" Control.Monad.State
import Data.List
import Data.Maybe

runPT :: Monad m => PTMonad m a -> ObjectEnv m -> m ( a, ObjectEnv m )
runPT = runObject

initPTEnv :: ObjectEnv m
initPTEnv = initObjectEnv

type PTMonad m = ObjectMonad m

type Object = ObjectId
type Member = VarName

liftPT :: Monad m => m a -> ObjectMonad m a
liftPT = lift

makeMember :: Monad m => String -> ObjectMonad m VarName
makeMember = mkVarName

method :: Monad m =>
	ObjectId -> VarName -> [ ObjectId ] -> ObjectMonad m [ ObjectId ]
method = sendMsg

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

primInt :: Int -> ObjectId
primInt = primitiveInt

primStr :: String -> ObjectId
primStr = primitiveString

fromPrimInt :: ObjectId -> Int
fromPrimInt = fromPrimitiveInt

fromPrimStr :: ObjectId -> String
fromPrimStr = fromPrimitiveString

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

printMemberName :: MonadIO m => VarName -> m ()
printMemberName = printVarName
printVarName :: MonadIO m => VarName -> m ()
printVarName ( VarName vn ) = liftIO $ putStrLn vn

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

setMember :: Monad m => ObjectId -> VarName -> ObjectId -> ObjectMonad m ()
setMember = setVar

setVar :: Monad m => ObjectId -> VarName -> ObjectId -> ObjectMonad m ()
setVar obj vn val = do
	st <- getObject obj >>= return . objectStatus
	putObject $ Object obj $ ( vn, val ) : st

member :: Monad m => ObjectId -> VarName -> ObjectMonad m ObjectId
member = getVar
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
