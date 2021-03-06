{-# LANGUAGE PackageImports #-}

module Control.Prototype (
	Prot,
	ProtEnv,
	Object,
	Member,
	Method,

	runProt,
	evalProt,
	execProt,
	initProtEnv,
	package,
	object,
	clone,
	makeMember,
	setMember,
	member,
	makeMethod,
	setMethod,
	method,

	liftProt,
	primBool,
	primInt,
	primStr,
	fromPrimBool,
	fromPrimInt,
	fromPrimStr,

	printMember,
) where

import Prelude hiding ( head )
import "monads-tf" Control.Monad.State (
	StateT, runStateT, evalStateT, execStateT,
	lift, MonadIO, liftIO, put, get, gets )
import Data.List ( (\\) )
import Data.Maybe ( fromMaybe, listToMaybe )

head :: a -> [ a ] -> a
head x = fromMaybe x . listToMaybe

mmap :: Monad m => ( a -> b ) -> m a -> m b
mmap f mx = mx >>= return . f

--------------------------------------------------------------------------------

type Prot m = StateT ( ProtEnv m ) m

runProt :: Monad m => Prot m a -> ProtEnv m -> m ( a, ProtEnv m )
runProt = runStateT

evalProt :: Monad m => Prot m a -> ProtEnv m -> m a
evalProt = evalStateT

execProt :: Monad m => Prot m a -> ProtEnv m -> m ( ProtEnv m )
execProt = execStateT

liftProt :: Monad m => m a -> Prot m a
liftProt = lift

data ProtEnv m = ProtEnv {
	packageName	:: String,
	objectBodys	:: [ ObjectBody m ]
 } deriving Show

initProtEnv :: ProtEnv m
initProtEnv = ProtEnv {
	packageName	= "main",
	objectBodys	= [ ObjectBody object [ ] ]
 }

data Object =
	ObjectId { fromObjId :: Int } |
	PrimitiveBool { fromPrimBool :: Bool } |
	PrimitiveInt { fromPrimInt :: Int } |
	PrimitiveString { fromPrimStr :: String }
	deriving ( Eq, Show )

primBool :: Bool -> Object
primBool = PrimitiveBool

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
type Method m = Object -> [ Object ] -> Prot m [ Object ]

instance Show ( ObjectBody m ) where
	show ( Method oid _ ) = "Method " ++ show oid
	show ( ObjectBody oid st ) = "Object " ++ show oid ++ " " ++ show st

--------------------------------------------------------------------------------

getNewId :: Monad m => Prot m Object
getNewId = gets $ ObjectId . head err . ( [ 1 .. ] \\ ) .
	map ( fromObjId . objectId ) . objectBodys
	where
	err = error "too many objects"

getObject :: Monad m => Object -> Prot m ( ObjectBody m )
getObject obj =	gets $ head err . filter ( ( == obj ) . objectId ) . objectBodys
	where
	err = error $ "no such object: " ++ show obj

putObject :: Monad m => ObjectBody m -> Prot m ()
putObject objBody = do
	env@ProtEnv { objectBodys = obs } <- get
	put env { objectBodys = objBody : obs }

clone :: Monad m => Object -> Prot m Object
clone obj = do
	newId <- getNewId
	objBody <- getObject obj
	putObject $ objBody { objectId = newId }
	return newId

makeMember :: Monad m => String -> Prot m Member
makeMember name =
	mmap ( Member . ( ++ name ) . ( ++ "::" ) ) $ gets packageName

member :: Monad m => Object -> Member -> Prot m Object
member obj mem =
	mmap ( fromMaybe err . lookup mem . objectMembers ) $ getObject obj
	where
	err = error $ "No such member: " ++ show mem ++ "\nobject: " ++ show obj

method :: Monad m => Object -> Member -> [ Object ] -> Prot m [ Object ]
method obj mem args =
	member obj mem >>= getObject >>= ( $ args ) . ( $ obj ) . objectMethod

setMember :: Monad m => Object -> Member -> Object -> Prot m ()
setMember obj mn val = do
	objBody@ObjectBody { objectMembers = mems } <- getObject obj
	putObject objBody { objectMembers = ( mn, val ) : mems }

makeMethod :: Monad m => Method m -> Prot m Object
makeMethod met = do
	newId <- getNewId
	putObject $ Method newId met
	return newId

setMethod :: Monad m => Object -> Member -> Method m -> Prot m ()
setMethod obj mem met = do
	newId <- getNewId
	putObject $ Method newId met
	setMember obj mem newId

package :: Monad m => String -> Prot m a -> Prot m a
package pkg act = do
	env0@ProtEnv { packageName = oldPkg } <- get
	put env0 { packageName = pkg }
	ret <- act
	env1 <- get
	put env1 { packageName = oldPkg }
	return ret

printMember :: MonadIO m => Member -> m ()
printMember ( Member name ) = liftIO $ putStrLn name
