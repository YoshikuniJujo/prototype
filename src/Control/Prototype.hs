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

	package,

	liftPT,
	primInt,
	primStr,
	fromPrimInt,
	fromPrimStr,

	printMemberName,
) where

import Prelude hiding ( head )
import "monads-tf" Control.Monad.State (
	StateT, runStateT, lift, MonadIO, liftIO, put, get, gets )
import Data.List ( (\\) )
import Data.Maybe ( fromMaybe, listToMaybe )

head :: a -> [ a ] -> a
head x = fromMaybe x . listToMaybe

mmap :: Monad m => ( a -> b ) -> m a -> m b
mmap f mx = mx >>= return . f

--------------------------------------------------------------------------------

type PTMonad m = StateT ( PTEnv m ) m

runPT :: Monad m => PTMonad m a -> PTEnv m -> m ( a, PTEnv m )
runPT = runStateT

liftPT :: Monad m => m a -> PTMonad m a
liftPT = lift

data PTEnv m = PTEnv {
	packageName	:: String,
	objectBodys	:: [ ObjectBody m ]
 } deriving Show

initPTEnv :: PTEnv m
initPTEnv = PTEnv {
	packageName	= "main",
	objectBodys	= [ ObjectBody object [ ] ]
 }

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

getNewId :: Monad m => PTMonad m Object
getNewId = gets $ ObjectId . head err . ( [ 1 .. ] \\ ) .
	map ( fromObjId . objectId ) . objectBodys
	where
	err = error "too many objects"

getObject :: Monad m => Object -> PTMonad m ( ObjectBody m )
getObject obj =	gets $ head err . filter ( ( == obj ) . objectId ) . objectBodys
	where
	err = error $ "no such object: " ++ show obj

putObject :: Monad m => ObjectBody m -> PTMonad m ()
putObject objBody = do
	env@PTEnv { objectBodys = obs } <- get
	put env { objectBodys = objBody : obs }

clone :: Monad m => Object -> PTMonad m Object
clone obj = do
	newId <- getNewId
	objBody <- getObject obj
	putObject $ objBody { objectId = newId }
	return newId

makeMember :: Monad m => String -> PTMonad m Member
makeMember name =
	mmap ( Member . ( ++ name ) . ( ++ "::" ) ) $ gets packageName

member :: Monad m => Object -> Member -> PTMonad m Object
member obj mem =
	mmap ( fromMaybe err . lookup mem . objectMembers ) $ getObject obj
	where
	err = error $ "No such member: " ++ show mem ++ "\nobject: " ++ show obj

method :: Monad m => Object -> Member -> [ Object ] -> PTMonad m [ Object ]
method obj mem args =
	member obj mem >>= getObject >>= ( $ args ) . ( $ obj ) . objectMethod

setMember :: Monad m => Object -> Member -> Object -> PTMonad m ()
setMember obj mn val = do
	objBody@ObjectBody { objectMembers = mems } <- getObject obj
	putObject objBody { objectMembers = ( mn, val ) : mems }

setMethod :: Monad m => Object -> Member -> Method m -> PTMonad m ()
setMethod obj mem met = do
	newId <- getNewId
	putObject $ Method newId met
	setMember obj mem newId

package :: Monad m => String -> PTMonad m a -> PTMonad m a
package pkg act = do
	env0@PTEnv { packageName = oldPkg } <- get
	put env0 { packageName = pkg }
	ret <- act
	env1 <- get
	put env1 { packageName = oldPkg }
	return ret

printMemberName :: MonadIO m => Member -> m ()
printMemberName ( Member name ) = liftIO $ putStrLn name
