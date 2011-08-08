module Main where

import Android

main :: IO ()
main = runAndroid $ do
	( textView, setText, setContentView ) <- importAndroid
	myTV <- clone textView
	sendMsg myTV setText [ primitiveString "今日は、世界!" ]
	sendMsg myTV setContentView [ ]
	return ()
