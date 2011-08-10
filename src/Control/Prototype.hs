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

import Prelude hiding ( head )
import "monads-tf" Control.Monad.State (
	StateT, runStateT, lift, MonadIO, liftIO, modify, gets )
import Data.List ( (\\) )
import Data.Maybe ( fromMaybe, listToMaybe )

head :: [ a ] -> Maybe a
head = listToMaybe

--------------------------------------------------------------------------------

type PTMonad m = StateT ( PTEnv m ) m

runPT :: Monad m => PTMonad m a -> PTEnv m -> m ( a, PTEnv m )
runPT = runStateT

liftPT :: Monad m => m a -> PTMonad m a
liftPT = lift

type PTEnv m = [ ObjectBody m ]

initPTEnv :: PTEnv m
initPTEnv = [ ObjectBody object [ ] ]

data Object =
	ObjectId { fromObjId :: Int } |
	PrimitiveInt { fromPrimInt :: Int } |
	PrimitiveString { fromPrimStr :: String }
	deriving ( Eq, Show )

primInt :: Int -> Object
primInt = PrimitiveInt

primStr :: String -> Object
primStr = PrimitiveString

object :: Object
object = ObjectId 0

data ObjectBody m =
	ObjectBody {
		objectId	:: Object,
		objectMembers	:: [ ( Member, Object ) ] } |
	Method {
		objectId	:: Object,
		objectMethod	:: Method m }

data Member = Member String deriving ( Eq, Show )
type Method m = Object -> [ Object ] -> PTMonad m [ Object ]

instance Show ( ObjectBody m ) where
	show ( Method oid _ ) = "Method " ++ show oid
	show ( ObjectBody oid st ) = "Object " ++ show oid ++ " " ++ show st

--------------------------------------------------------------------------------

getObject :: Monad m => Object -> PTMonad m ( ObjectBody m )
getObject obj =
	gets ( fromMaybe err . head . filter ( ( == obj ) . objectId ) )
	where
	err = error $ "no such object: " ++ show obj

putObject :: Monad m => ObjectBody m -> PTMonad m ()
putObject = modify . ( : )

getNewId :: Monad m => PTMonad m Object
getNewId = do
	ids <- gets $ map ( fromObjId . objectId )
	return $ ObjectId $ fromMaybe err $ head $ [ 1 .. ] \\ ids
	where
	err = error "too many objects"

clone :: Monad m => Object -> PTMonad m Object
clone obj = do
	mems <- getObject obj >>= return . objectMembers
	newId <- getNewId
	putObject $ ObjectBody newId mems
	return newId

makeMember :: Monad m => String -> PTMonad m Member
makeMember = return . Member

member :: Monad m => Object -> Member -> PTMonad m Object
member obj mn = do
	ObjectBody { objectMembers = mems } <- getObject obj
	return $ fromMaybe err $ lookup mn mems
	where
	err = error $ "No such member: " ++ show mn ++ "\nobject: " ++ show obj

setMember :: Monad m => Object -> Member -> Object -> PTMonad m ()
setMember obj mn val = do
	ObjectBody { objectMembers = mems } <- getObject obj
	putObject $ ObjectBody obj $ ( mn, val ) : mems

method :: Monad m => Object -> Member -> [ Object ] -> PTMonad m [ Object ]
method obj mn args = do
	mid <- member obj mn
	Method { objectMethod = m } <- getObject mid
	m obj args

setMethod :: Monad m => Object -> String -> Method m -> PTMonad m Member
setMethod obj name m = do
	newId <- getNewId
	mn <- makeMember name
	putObject $ Method newId m
	setMember obj mn newId
	return mn

printMemberName :: MonadIO m => Member -> m ()
printMemberName ( Member vn ) = liftIO $ putStrLn vn
