module Main where

import Object

main :: IO ()
main = do
	putStrLn "yet"
	print $ runIdentity testObject

testObject :: Identity ( Int, ObjectEnv Identity )
testObject = runObject $ do
	tint <- getObjectType "int"
	mint <- getMethodName "int"
	setObjectMethod tint mint ( \_ _ -> return [ ] )
	newObject tint -- [ ObjectValueInt 8 ]
	return 123

{-
initInt :: ObjectMethod Identity
initInt this [ v ] = do
	num <- getObjectValueName "num"
	setObjectValue this num v
	return [ ]
-}
